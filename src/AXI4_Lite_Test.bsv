package AXI4_Lite_Test;

import StmtFSM :: *;

import AXI4_Lite_Master :: *;
import AXI4_Lite_Types :: *;

//testbench package containing convenient functions/FSMs

function Stmt axil_read(AXI4_Lite_Master_Rd#(a, d) m, Bit#(a) addr, Reg#(Bit#(d)) rg_out);
    return seq
        axi4_lite_read(m, addr);
        action
            let rsp <- axi4_lite_read_response(m);
            rg_out <= rsp;
        endaction
    endseq;
endfunction

function Stmt wait_bit_set(
        AXI4_Lite_Master_Rd#(32, 32) cfg_rd,
        Integer address,
        Integer bitpos,
        Reg#(Bit#(32)) rg_rsp
    );
    return seq
        rg_rsp <= 0;
        while (!unpack(rg_rsp[bitpos])) seq
            axil_read(cfg_rd, fromInteger(address), rg_rsp);
        endseq
    endseq;
endfunction

function Stmt wait_bit_not_set(
        AXI4_Lite_Master_Rd#(32, 32) cfg_rd,
        Integer address,
        Integer bitpos,
        Reg#(Bit#(32)) rg_rsp
    );
    return seq
        rg_rsp <= 0;
        while (unpack(rg_rsp[bitpos])) seq
            axil_read(cfg_rd, fromInteger(address), rg_rsp);
        endseq
    endseq;
endfunction

endpackage