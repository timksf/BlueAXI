package TestBlueBus;

import StmtFSM :: *;

import BlueLib :: *;
import BlueAXI :: *;

interface ModConfig_ifc;
    method Bit#(12) field0;
    method Bit#(4) field1;
endinterface

module [BlueBusCtx_t#(32)] module_config(ModConfig_ifc);

    blue_reg_def('h00, "MID", "Module ID");
    blue_reg_co('h00, Bit#(12)'('hABC), 0, "ID", "Module ID", "Unique ID for this module");

    blue_reg_def('h04, "CTRL", "Module control register");
    blue_reg_def('h08, "STS", "Module status register");

endmodule

module mkTestBlueBus(Empty);

    BlueBusAccess_ifc#(32, 32, ModConfig_ifc) cfg <- create_blue_bus(module_config);

    RegMapDoc_t#(32) doc <- doc_blue_bus(module_config);

    messageM(doc.reg_defs);

    Stmt s = seq
        printColorTimed(BLUE, $format("Hello World!"));
        $display("MID: %0x", cfg.internal.field0);
    endseq;

    mkAutoFSM(s);

endmodule

endpackage