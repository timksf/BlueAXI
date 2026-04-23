package TestBlueBus;

import StmtFSM :: *;

import BlueLib :: *;
import BlueAXI :: *;

typedef enum { Mode0, Mode1, Mode2 } Mode_t deriving(Bits, Eq, FShow);

interface ModConfig_ifc;
    method Bool en;
    method Mode_t mode;
endinterface


module [BlueBusCtx_t#(32)] module_config(ModConfig_ifc);

    Empty e = ?;
    Reg#(Bool)      rg_ctrl_en;
    Reg#(Mode_t)    rg_ctrl_mode;

    blue_reg_def('h00, "MIV", "Module ID and Version Register");
    e <- blue_reg_co('h00, Bit#(12)'('hABC),   0, "MID", "Module ID",       "Unique ID for this module.");
    e <- blue_reg_co('h00, Bit#(12)'('hDDA),  16, "VRS", "Module Version",  "Module release version.");

    blue_reg_def('h04, "CTRL", "Module control register");
    rg_ctrl_en   <- blue_reg_rw('h04, False, 0, "CTRLEN",    "Control Enable",       "Controls whether module is enabled or not.");
    rg_ctrl_mode <- blue_reg_rw('h04, Mode1, 4, "MODE",      "Control Mode Setting", "Controls operating mode.");

    blue_reg_def('h08, "STS", "Module status register");

    method en   = rg_ctrl_en;
    method mode = rg_ctrl_mode;

endmodule

module mkTestBlueBus(Empty);

    BlueBusAccess_ifc#(32, 32, ModConfig_ifc) cfg <- create_blue_bus(module_config);
    BlueBusExport_ifc rdl_export <- export_systemrdl_blue_bus(module_config, "sim/testbluebus.rdl");

    RegMapDoc_t#(32) doc <- doc_blue_bus(module_config);

    messageM(doc.reg_defs);

    Stmt s = seq
        printColorTimed(BLUE, $format("Hello World!"));

        while(!rdl_export.done) noAction;

        action
            if(!rdl_export.success) begin
                printColorTimed(RED, $format("SystemRDL export failed."));
                $finish();
            end
            printColorTimed(GREEN, $format("SystemRDL export completed."));
        endaction

        $display("BUS[0x00]: %08x", cfg.external.read_pure(0));
        $display("BUS[0x04] reset: %08x", cfg.external.read_pure(4));

        action
            Bit#(32) ctrl_reset = cfg.external.read_pure(4);
            if(ctrl_reset != 'h00000010) begin
                printColorTimed(RED, $format("Sanity fail: reset CTRL expected 0x00000010 got %08x", ctrl_reset));
                $finish();
            end
        endaction

        //set CTRLEN=1 (bit 0) and MODE=Mode2 (bits 5:4 => b10)
        cfg.external.write_strobed('h04, 'h00000021, 'b1111);

        action
            Bit#(32) ctrl_after = cfg.external.read_pure(4);
            if(ctrl_after != 'h00000021) begin
                printColorTimed(RED, $format("Sanity fail: CTRL write expected 0x00000021 got %08x", ctrl_after));
                $finish();
            end
            if(cfg.internal.en != True) begin
                printColorTimed(RED, $format("Sanity fail: internal en expected True"));
                $finish();
            end
            if(cfg.internal.mode != Mode2) begin
                printColorTimed(RED, $format("Sanity fail: internal mode expected Mode2"));
                $finish();
            end
        endaction

        //strobe only byte 1; byte 0 fields must remain unchanged.
        cfg.external.write_strobed('h04, 'h0000AA00, 'b0010);

        action
            Bit#(32) ctrl_strobe = cfg.external.read_pure(4);
            if(ctrl_strobe != 'h00000021) begin
                printColorTimed(RED, $format("Sanity fail: strobed write changed CTRL unexpectedly: %08x", ctrl_strobe));
                $finish();
            end
            printColorTimed(GREEN, $format("Sanity pass: CTRL write/read/strobe behavior verified."));
        endaction
    endseq;

    mkAutoFSM(s);

endmodule

endpackage