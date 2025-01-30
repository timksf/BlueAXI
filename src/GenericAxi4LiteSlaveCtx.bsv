package GenericAxi4LiteSlaveCtx;

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

import AXI4_Lite_Types :: *;
import AXI4_Lite_Slave :: *;

//context for AXI4-Lite config module
typedef ModuleCollect#(StateOperator#(aw, dw), ifc) ConfigCtx#(numeric type aw, numeric type dw, type ifc);

typedef struct {
    Bit#(aw) offset;
    function ActionValue#(Bit#(dw)) _() fun;
} ReadOperation#(numeric type aw, numeric type dw);

typedef struct {
    Bit#(aw) offset;
    function Action _(Bit#(dw) d, Bit#(TDiv#(dw, 8)) s) fun;
} WriteOperation#(numeric type aw, numeric type dw);

typedef struct {
    Bit#(aw) offset;
    Bit#(aw) length;
    function Action _(Bit#(aw) a) request;
    function ActionValue#(Bit#(dw)) _() response;
} ReadRangeDelayedOperation#(numeric type aw, numeric type dw);

typedef struct {
    Bit#(aw) offset;
    Bit#(aw) length;
    function Action _(Bit#(aw) a, Bit#(dw) d, Bit#(TDiv#(dw, 8)) s) request;
} WriteRangeOperation#(numeric type aw, numeric type dw);

//"generic" operator type
typedef union tagged {
    WriteRangeOperation#(aw, dw) WriteRangeOperator;
    ReadRangeDelayedOperation#(aw, dw) ReadRangeDelayedOperator;
    WriteOperation#(aw, dw) WriteOperator;
    ReadOperation#(aw, dw) ReadOperator;
} StateOperator#(numeric type aw, numeric type dw);

function Bit#(aw) max_op(StateOperator#(aw, dw) a, Bit#(aw) b);
    let v = 0;
    case(a) matches
        tagged WriteRangeOperator .x: 
            v = x.offset+x.length;
        tagged ReadRangeDelayedOperator .x: 
            v = x.offset+x.length;
        tagged WriteOperator .x: 
            v = x.offset;
        tagged ReadOperator .x: 
            v = x.offset;
    endcase
    if(v > b) 
        return v;
    else 
        return b;
endfunction

function Integer bit_to_integer(Bit#(n) x);
    Integer res = 0;
    for (Integer i=0; i<valueOf(n); i=i+1)
        if (x[i] == 1)
            res = res + 2**i;
    return res;
endfunction

//-------------------------------------------------------------------//
//use these functions for aggregation of operators of same kind

function List#(ReadOperation#(aw, dw)) getReadOperator(StateOperator#(aw, dw) state_op);
    return state_op matches tagged ReadOperator .rr ? Cons(rr, Nil) : Nil;
endfunction

function List#(WriteOperation#(aw, dw)) getWriteOperator(StateOperator#(aw, dw) state_op);
    return state_op matches tagged WriteOperator .rr ? Cons(rr, Nil) : Nil;
endfunction

function List#(ReadRangeDelayedOperation#(aw, dw)) getReadRangeDelayedOperator(StateOperator#(aw, dw) state_op);
    return state_op matches tagged ReadRangeDelayedOperator .rr ? Cons(rr, Nil) : Nil;
endfunction

function List#(WriteRangeOperation#(aw, dw)) getWriteRangeOperator(StateOperator#(aw, dw) state_op);
    return state_op matches tagged WriteRangeOperator .rr ? Cons(rr, Nil) : Nil;
endfunction

//-------------------------------------------------------------------//
//modules for populating context

function ActionValue#(Bit#(dw)) register_read(Reg#(t) r) provisos(Bits#(t, a__)) = actionvalue return cExtend(r); endactionvalue;

function Action register_write_strobed(Reg#(t) r, Bit#(dw) d, Bit#(b__) strobe) provisos(Bits#(t, a__), Mul#(b__, 8, dw), Div#(dw, 8, b__));
    action
        Vector#(TDiv#(dw, 8), Bit#(8)) tr = cExtend(r);
        Vector#(TDiv#(dw, 8), Bit#(8)) td = unpack(d);
        for(Integer i = 0; i < valueOf(b__); i = i + 1) begin
            if(unpack(strobe[i])) begin
                tr[i] = td[i];
            end
        end
        r <= cExtend(tr);
    endaction
endfunction

function ActionValue#(Bit#(dw)) fifo_read(FIFO#(t) f) provisos(Bits#(t, a__));
    actionvalue
        let x = f.first(); f.deq();
        return cExtend(x);
    endactionvalue
endfunction

function Action fifo_write(FIFO#(t) f, Bit#(dw) d) provisos(Bits#(t, a__));
    action 
        f.enq(cExtend(d));
    endaction
endfunction

function Action fifo_write_strobed(FIFO#(t) f, Bit#(dw) d, Bit#(b__) strobe) provisos(Bits#(t, a__), Mul#(b__, 8, dw), Div#(dw, 8, b__));
     action
        Vector#(TDiv#(dw, 8), Bit#(8)) tr = replicate(0);
        Vector#(TDiv#(dw, 8), Bit#(8)) td = unpack(d);
        for(Integer i = 0; i < valueOf(b__); i = i + 1) begin
            if(unpack(strobe[i])) begin
                tr[i] = td[i];
            end
        end
        f.enq(cExtend(tr));
    endaction
endfunction

function Action bram_read_request(Integer offset, BRAMServerBE#(a, b, c) bram, Bit#(aw) addr) provisos(Bits#(a, a_sz), Bits#(b, b_sz));
    action
        addr = addr - fromInteger(offset);
        Bit#(a_sz) regNum = zExtend(addr >> valueOf(TLog#(TDiv#(b_sz, 8))));
        bram.request.put(BRAMRequestBE {writeen: 0, responseOnWrite: False, address: unpack(regNum), datain: unpack(0)});
    endaction
endfunction

function ActionValue#(Bit#(dw)) bram_read_response(BRAMServerBE#(a, b, c) bram) provisos(Bits#(b, b__));
    actionvalue
        let data <- bram.response.get();
        return cExtend(data);
    endactionvalue
endfunction

function Action bram_write_request(Integer offset, BRAMServerBE#(a, b, c) bram, Bit#(aw) addr, Bit#(dw) d, Bit#(c) s)
    provisos(Bits#(a, a_sz), Bits#(b, b_sz));
    action
        addr = addr - fromInteger(offset);
        Bit#(a_sz) regNum = zExtend(addr >> valueOf(TLog#(TDiv#(b_sz, 8))));
        bram.request.put(BRAMRequestBE {writeen: s, responseOnWrite: False, address: unpack(regNum), datain: cExtend(d)});
    endaction
endfunction

module [ConfigCtx#(aw, dw)] addValueRO#(Integer offset, t v)() provisos(Bits#(t, a__));

    Bit#(aw) offs = fromInteger(offset);
    ActionValue#(Bit#(dw)) my_read = actionvalue return cExtend(v); endactionvalue;
    addToCollection(tagged GenericAxi4LiteSlaveCtx::ReadOperator ReadOperation { offset: offs, fun: my_read } );

endmodule

module [ConfigCtx#(aw, dw)] addRegRO#(Integer offset, Reg#(t) r)() provisos(Bits#(t, a__));

    Bit#(aw) offs = fromInteger(offset);
    ActionValue#(Bit#(dw)) my_read = register_read(r);
    addToCollection(tagged GenericAxi4LiteSlaveCtx::ReadOperator ReadOperation { offset: offs, fun: my_read } );

endmodule

module [ConfigCtx#(aw, dw)] addRegWO#(Integer offset, Reg#(t) r)()
    provisos(
        Bits#(t, a__),
        Mul#(b__, 8, dw),
        Div#(dw, 8, b__)
    );

    Bit#(aw) offs = fromInteger(offset);
    function Action my_write(Bit#(dw) d, Bit#(b__) strobe) = register_write_strobed(r, d, strobe);
    addToCollection(tagged GenericAxi4LiteSlaveCtx::WriteOperator WriteOperation { offset: offs, fun: my_write } );

endmodule

module [ConfigCtx#(aw, dw)] addReg#(Integer offset, Reg#(t) r)()
    provisos(
        Bits#(t, a__),
        Mul#(b__, 8, dw),
        Div#(dw, 8, b__)
    );

    Bit#(aw) offs = fromInteger(offset);
    function Action my_write(Bit#(dw) d, Bit#(b__) strobe) = register_write_strobed(r, d, strobe);
    ActionValue#(Bit#(dw)) my_read = register_read(r);
    addToCollection(tagged GenericAxi4LiteSlaveCtx::WriteOperator WriteOperation { offset: offs, fun: my_write } );
    addToCollection(tagged GenericAxi4LiteSlaveCtx::ReadOperator ReadOperation { offset: offs, fun: my_read } );

endmodule

module [ConfigCtx#(aw, dw)] addFifoRO#(Integer offset, FIFO#(t) f)() 
    provisos(
        Bits#(t, a__),
        Mul#(b__, 8, dw),
        Div#(dw, 8, b__)
    );

    Bit#(aw) offs = fromInteger(offset);
    ActionValue#(Bit#(dw)) my_read = fifo_read(f);
    addToCollection(tagged GenericAxi4LiteSlaveCtx::ReadOperator ReadOperation { offset: offs, fun: my_read } );

endmodule

module [ConfigCtx#(aw, dw)] addFifoWO#(Integer offset, FIFO#(t) f)() 
    provisos(
        Bits#(t, a__),
        Mul#(b__, 8, dw),
        Div#(dw, 8, b__)
    );

    Bit#(aw) offs = fromInteger(offset);
    function Action my_write(Bit#(dw) d, Bit#(b__) strobe) = fifo_write_strobed(f, d, strobe);
    addToCollection(tagged GenericAxi4LiteSlaveCtx::WriteOperator WriteOperation { offset: offs, fun: my_write } );

endmodule

module [ConfigCtx#(aw, dw)] addBramRO#(Integer offset, Integer length, BRAMServerBE#(a, b, c) bram)()
    provisos(
        Bits#(a, a__),
        Bits#(b, b__)
    );

    Bit#(aw) offs = fromInteger(offset);
    Bit#(aw) len = fromInteger(length);
    function Action my_read_req(Bit#(aw) a) = bram_read_request(offset, bram, a);
    ActionValue#(Bit#(dw)) my_read_resp = bram_read_response(bram);
    let my_op = ReadRangeDelayedOperation { offset: offs, length: len, request: my_read_req, response: my_read_resp };
    addToCollection(tagged GenericAxi4LiteSlaveCtx::ReadRangeDelayedOperator my_op);

endmodule

module [ConfigCtx#(aw, dw)] addBramWO#(Integer offset, Integer length, BRAMServerBE#(a, b, c) bram)()
    provisos(
        Bits#(a, a__),
        Bits#(b, b__)
    );

    Bit#(aw) offs = fromInteger(offset);
    Bit#(aw) len = fromInteger(length);
    function Action my_write_req(Bit#(aw) a, Bit#(dw) d, Bit#(TDiv#(dw, 8)) s) = bram_write_request(offset, bram, a, d, cExtend(s));
    let my_op = WriteRangeOperation { offset: offs, length: len, request: my_write_req };
    addToCollection(tagged GenericAxi4LiteSlaveCtx::WriteRangeOperator my_op);

endmodule

module [ConfigCtx#(aw, dw)] addBramSinglePort#(Integer offset, Integer length, BRAMServerBE#(a, b, c) bram)()
    provisos(
        Bits#(a, a__),
        Bits#(b, b__)
    );
    //this will lead to conflicts between read and write rules, where the compiler decides which it prefers over the other
    addBramRO(offset, length, bram);
    addBramWO(offset, length, bram);

endmodule

module [ConfigCtx#(aw, dw)] addBramDualPort#(Integer offset, Integer length, BRAM2PortBE#(a, b, c) bram)()
    provisos(
        Bits#(a, a__),
        Bits#(b, b__)
    );

    addBramRO(offset, length, bram.portA);
    addBramWO(offset, length, bram.portB);

endmodule


//-------------------------------------------------------------------//

//AXI config interface has the usual read and write slaves
interface AXI4LiteConfig_ifc#(numeric type aw, numeric type dw);
    interface AXI4_Lite_Slave_Rd_Fab#(aw, dw) s_rd;
    interface AXI4_Lite_Slave_Wr_Fab#(aw, dw) s_wr;
endinterface

//interface combining the external facing AXI bus interface with an internal device facing interface
interface IntExtConfig_ifc#(numeric type aw, numeric type dw, type internal_ifc);
    interface AXI4LiteConfig_ifc#(aw, dw) bus_ifc;
    interface internal_ifc device_ifc;
endinterface

//actual config module accummulating the whole context
module [Module] axi4LiteConfigFromContext#(ConfigCtx#(aw, dw, i) ctx)(IntExtConfig_ifc#(aw, dw, i));

    AXI4_Lite_Slave_Rd#(aw, dw) slave_rd <- mkAXI4_Lite_Slave_Rd(2);
    AXI4_Lite_Slave_Wr#(aw, dw) slave_wr <- mkAXI4_Lite_Slave_Wr(2);

    //control register for delayed operations
    Reg#(Bool) read_busy <- mkReg(False);

    let {coll_device_ifc, c} <- getCollection(ctx);
    let items_read = List::concat(List::map(getReadOperator, c));
    let items_write = List::concat(List::map(getWriteOperator, c));
    let items_read_range_delayed = List::concat(List::map(getReadRangeDelayedOperator, c));
    let items_write_range = List::concat(List::map(getWriteRangeOperator, c));

    Integer highest_addr = bit_to_integer(List::foldr(max_op, 0, c)) + 1;
    Bit#(aw) byte_escape = ((1 << valueOf(TLog#(TDiv#(dw, 8)))) - 1); //used for correction unaligned accesses
    Bit#(aw) addr_escape = ((1 << log2(highest_addr)) - 1) - byte_escape;

    messageM("highest addr: " + integerToString(highest_addr));
    messageM("byte escape: " + integerToString(bit_to_integer(byte_escape)));
    messageM("addr escape: " + integerToString(bit_to_integer(addr_escape)));

    Rules read_rules = emptyRules();
    Rules write_rules = emptyRules();

    Integer num_reads = length(items_read);
    for(Integer i = 0; i < num_reads; i = i + 1) begin
        let read_op = items_read[i];
        read_rules = rJoinMutuallyExclusive(rules
            rule rread (read_op.offset == (slave_rd.first().addr & addr_escape) && !read_busy);
                let req <- slave_rd.request.get();
                Bit#(dw) retVal <- read_op.fun();
                AXI4_Lite_Read_Rs_Pkg#(dw) response;
                response.resp = OKAY;
                response.data = retVal;
                slave_rd.response.put(response);
            endrule
        endrules, read_rules);
    end

    function Bool in_range(Bit#(aw) a, Bit#(aw) min, Bit#(aw) len);
        return a >= min && a < min + len;
    endfunction

    //delayed range reads
    Integer num_reads_range_delayed = length(items_read_range_delayed);
    for(Integer i = 0; i < num_reads_range_delayed; i = i + 1) begin
        let read_op = items_read_range_delayed[i];
        let req_addr = slave_rd.first().addr & addr_escape;
        let addr_valid = in_range(req_addr, read_op.offset, read_op.length);
        read_rules = rJoinMutuallyExclusive(rules
            rule rrelay_read_req (addr_valid && !read_busy);            
                read_op.request(req_addr);
                read_busy <= True;
            endrule

            rule rpass_delayed_resp (addr_valid && read_busy);
                let req <- slave_rd.request.get();
                let data <- read_op.response();
                AXI4_Lite_Read_Rs_Pkg#(dw) response;
                response.resp = OKAY;
                response.data = data;
                slave_rd.response.put(response);
                read_busy <= False;
            endrule
        endrules, read_rules);
    end

    read_rules = rJoinDescendingUrgency(read_rules, rules
        //catches reads of unpopulated addresses as well as reads
        //beyond the defined range of a state element
        rule rread_invalid (!read_busy);
            let req <- slave_rd.request.get();
            AXI4_Lite_Read_Rs_Pkg#(dw) response;
            //address not populated, just return DECERR
            response.data = fromInteger(-1);
            response.resp = OKAY;
            slave_rd.response.put(response);
            $display("slave: address %h not populated with readable state element", req.addr);
        endrule
    endrules);

    //writes
    Integer num_writes = length(items_write);
    for(Integer i = 0; i < num_writes; i = i + 1) begin
        let write_op = items_write[i];
        write_rules = rJoinMutuallyExclusive(rules
            rule rwrite (write_op.offset == (slave_wr.first().addr & addr_escape));
                let req <- slave_wr.request.get();
                write_op.fun(req.data, req.strb);
                AXI4_Lite_Write_Rs_Pkg response;
                response.resp = OKAY;
                slave_wr.response.put(response);
            endrule
        endrules, write_rules);
    end

    //write ranges
    Integer num_writes_range = length(items_write_range);
    for(Integer i = 0; i < num_writes_range; i = i + 1) begin
        let write_op = items_write_range[i];
        let req_addr = slave_wr.first().addr & addr_escape;
        let addr_valid = in_range(req_addr, write_op.offset, write_op.length);
        write_rules = rJoinMutuallyExclusive(rules
            rule rwrite_range (addr_valid);
                let req <- slave_wr.request.get();
                write_op.request(req_addr, req.data, req.strb);
                AXI4_Lite_Write_Rs_Pkg response;
                response.resp = OKAY;
                slave_wr.response.put(response);
            endrule
        endrules, write_rules);
    end

    write_rules = rJoinDescendingUrgency(write_rules, rules
        rule rwrite_invalid;
            let req <- slave_wr.request.get();
            AXI4_Lite_Write_Rs_Pkg response;
            //address not populated, just return DECERR
            response.resp = DECERR;
            slave_wr.response.put(response);
            $display("slave: address %h not populated with writable state element", req.addr);
        endrule
    endrules);


    addRules(read_rules);
    addRules(write_rules);

    interface AXI4LiteConfig_ifc bus_ifc;
        interface s_rd = slave_rd.fab;
        interface s_wr = slave_wr.fab;
    endinterface

    interface device_ifc = coll_device_ifc;

endmodule

endpackage