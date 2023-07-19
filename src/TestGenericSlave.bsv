package TestCtx;

import StmtFSM :: *;
import FIFO :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import ClientServer :: *;
import GetPut :: *;
import Connectable :: *;
import List :: *;
import ModuleCollect :: *;
import BUtils :: *;
import Vector :: *;
import BRAM :: *;

import AXI4_Lite_Master :: *;
import AXI4_Lite_Slave :: *;
import AXI4_Lite_Types :: *;

import BlueLib :: *;


interface ConfigDev_ifc;
    //nothing for now
    interface Reg#(Int#(32)) opt1;
    interface Get#(Int#(32)) fifo_in; //a write-only fifo serves as input
    interface Put#(Int#(32)) fifo_out; //a read-only fifo serves as output
endinterface

module [ConfigCtx#(aw, dw)] generateMyConfig(ConfigDev_ifc) provisos(Mul#(TDiv#(dw, 8), 8, dw));

    Reg#(Int#(32)) magic_number <- mkReg(-1337);
    Reg#(Int#(32)) magic_number_1 <- mkReg(42);

    Reg#(Int#(32)) cfg_opt1 <- mkRegU;

    FIFO#(Int#(32)) cfg_fifo <- mkFIFO;
    FIFO#(Int#(32)) cfg_fifo1 <- mkFIFO;

    BRAM_Configure cfg = defaultValue;
    cfg.loadFormat = tagged Hex "bram_init.txt";
    BRAM2PortBE#(Bit#(8), Bit#(32), 4) bram <- mkBRAM2ServerBE(cfg);
    BRAM2PortBE#(Bit#(8), Bit#(32), 4) bram2 <- mkBRAM2ServerBE(cfg);

    //add arbitrary config options 
    addRegRO('h27, asReg(magic_number));
    addRegRO('h28, asReg(magic_number_1));
    addReg('h29, asReg(cfg_opt1));

    addFifoRO('h30, cfg_fifo);
    addFifoWO('h34, cfg_fifo1);

    addBramRO('h40, 128, bram.portA); //read-only range for axi port
    addBramWO(196, 128, bram.portB); //write-only range for axi port

    addBramDualPort('h200, 256, bram2);

    interface opt1 = cfg_opt1;
    interface fifo_in = toGet(cfg_fifo1);
    interface fifo_out = toPut(cfg_fifo);

endmodule

module mkTestCtx();

    IntExtConfig_ifc#(32, 32, ConfigDev_ifc) config_module <- axi4LiteConfigFromContext(generateMyConfig);
    AXI4LiteConfig_ifc#(32, 32) axi4config = config_module.bus_ifc;

    AXI4_Lite_Master_Rd#(32, 32) rd <- mkAXI4_Lite_Master_Rd(8);
    AXI4_Lite_Master_Wr#(32, 32) wr <- mkAXI4_Lite_Master_Wr(8);

    mkConnection(axi4config.s_rd, rd.fab);
    mkConnection(axi4config.s_wr, wr.fab);

    Reg#(UInt#(32)) responses <- mkReg(0);

    function Action expectOkay(AXI4_Lite_Response resp) = action
        if(resp != OKAY) begin
            printColorTimed(RED, $format("Bus Error"));
            $finish();
        end
    endaction;

    function Action expectError(AXI4_Lite_Response resp) = action
        if(resp == OKAY) begin
            printColorTimed(RED, $format("Expected Bus Error"));
            $finish();
        end
    endaction;

    Stmt s = seq
        par
            seq
            axi4_lite_read(rd, 'h27);
            axi4_lite_read(rd, 'h28);
            endseq
            while(responses < 2) seq
                action
                    let resp <- axi4_lite_read_response(rd);
                    Int#(32) casted = unpack(resp);
                    printColorTimed(GREEN, $format("Got reponse: %d", casted));
                endaction
                responses <= responses + 1;
            endseq
        endpar
        par 
            seq
            axi4_lite_write(wr, 'h29, 'hDEADBEEF);
            axi4_lite_read(rd, 'h29);
            endseq
            seq
            action
            let resp <- axi4_lite_write_response(wr);
            endaction
            action
                let resp <- axi4_lite_read_response(rd);
                printColorTimed(GREEN, $format("Got reponse: %H", resp));
            endaction
            endseq
        endpar
        //test invalid read
        axi4_lite_read(rd, 'h31);
        action
            let r <- rd.response.get;
            expectError(r.resp);
        endaction
        //read from BRAM slave (initialized from text file)
        axi4_lite_read(rd, 'h40);
        action 
            let r <- axi4_lite_read_response(rd);
            printColorTimed(GREEN, $format("Got reponse: %H", r));
        endaction
        //write to BRAM slave
        axi4_lite_write(wr, 'h200, 'hDEADC0DE);
        action 
            let r <- axi4_lite_write_response(wr);
            expectOkay(r);
        endaction
        //write beyond BRAM range
        axi4_lite_write(wr, 'h200 + 256, 'hBAD);
        action
            let r <- axi4_lite_write_response(wr);
            expectError(r);
        endaction
        //simple BRAM read
        axi4_lite_read(rd, 'h200);
        action 
            let r <- axi4_lite_read_response(rd);
            printColorTimed(GREEN, $format("Got reponse: %H", r));
        endaction
        //read beyond BRAM range
        axi4_lite_read(rd, 'h200 + 256);
        action 
            let r <- rd.response.get;
            expectError(r.resp);
        endaction

        //push data into fifo from device interface and read with AXI interface
        config_module.device_ifc.fifo_out.put(133742);
        axi4_lite_read(rd, 'h30);
        action 
            let r <- axi4_lite_read_response(rd);
            printColorTimed(GREEN, $format("Got reponse: %d", r));
        endaction

        //push data into fifo from AXI side and read from device side
        responses <= 0;
        axi4_lite_write(wr, 'h34, -1700);
        action 
            let r <- axi4_lite_write_response(wr);
        endaction
        action
            let r <- config_module.device_ifc.fifo_in.get();
            printColorTimed(GREEN, $format("Got reponse: %d", r));
        endaction
    endseq;

    mkAutoFSM(s);

endmodule

endpackage