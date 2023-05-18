package TestGenericSlave;

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

import BlueLib :: *;
import BlueAXI :: *;

import GenericAXI4LiteSlaveCtx :: *;

module [ConfigCtx#(aw, dw)] generateMyConfig() provisos(Mul#(TDiv#(dw, 8), 8, dw));

    Reg#(Int#(32)) magic_number <- mkReg(-1337);
    Reg#(Int#(32)) magic_number_1 <- mkReg(42);

    Reg#(Int#(32)) cfg_opt1 <- mkRegU;

    FIFO#(Int#(32)) cfg_fifo <- mkFIFO;

    BRAM_Configure cfg = defaultValue;
    // cfg.loadFormat = tagged Hex "bram_init.txt";

    BRAM2PortBE#(Bit#(8), Bit#(32), 4) bram <- mkBRAM2ServerBE(cfg);
    BRAM2PortBE#(Bit#(8), Bit#(32), 4) bram2 <- mkBRAM2ServerBE(cfg);

    //add config options
    addRegRO('h27, asReg(magic_number));
    addRegRO('h28, asReg(magic_number_1));
    addReg('h29, asReg(cfg_opt1));

    addFifo('h30, cfg_fifo);
    addBramRO('h40, 128, bram.portA); //read-only range for axi port
    addBramWO(196, 128, bram.portB); //write-only range for axi port

    addBramDualPort('h200, 256, bram2);

endmodule

module mkTestCtx();

    AXI4LiteConfig_ifc#(32, 32) axi4config <- axi4LiteConfigFromContext(generateMyConfig);

    AXI4_Lite_Master_Rd#(32, 32) rd <- mkAXI4_Lite_Master_Rd(8);
    AXI4_Lite_Master_Wr#(32, 32) wr <- mkAXI4_Lite_Master_Wr(8);

    mkConnection(axi4config.s_rd, rd.fab);
    mkConnection(axi4config.s_wr, wr.fab);

    Reg#(UInt#(32)) responses <- mkReg(0);

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
        //test fifo
        responses <= 0;
        par
            seq
                axi4_lite_write(wr, 'h30, 'hDEADBEEF);
                axi4_lite_write(wr, 'h30, 'hBADCAFE);
            endseq
            while(responses < 2) 
                action
                    let r <- axi4_lite_write_response(wr);
                    responses <= responses + 1;
                endaction
        endpar
        responses <= 0;
        par
            seq
                axi4_lite_read(rd, 'h30);
                axi4_lite_read(rd, 'h30);
            endseq
            while(responses < 2) 
                action
                    let r <- axi4_lite_read_response(rd);
                    printColorTimed(GREEN, $format("Got reponse: %H", r));
                    responses <= responses + 1;
                endaction
        endpar
        axi4_lite_read(rd, 'h31);
        action
            let r <- rd.response.get;
            if(r.resp != DECERR)
                printColorTimed(YELLOW, $format("Expected error"));
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
        endaction
        axi4_lite_read(rd, 'h200);
        action 
            let r <- axi4_lite_read_response(rd);
            printColorTimed(GREEN, $format("Got reponse: %H", r));
        endaction
    endseq;

    mkAutoFSM(s);

endmodule

endpackage