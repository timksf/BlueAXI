package BlueBus;

import List :: *;
import BUtils :: *;
import ModuleCollect :: *;

interface BlueBus_ifc#(numeric type aw, numeric type dw);
    method Action write_strobed(Bit#(aw) addr, Bit#(dw) data, Bit#(TDiv#(dw, 8)) strobe);
    (* always_ready *)
    method Bit#(dw) read_pure(Bit#(aw) addr);
    method ActionValue#(Bit#(dw)) read_impure(Bit#(aw) addr);
endinterface

typedef ModuleCollect#(RegMapEntry#(dw), ifc) BlueBusCtx_t#(numeric type dw, type ifc);

typedef struct {
    Integer offset;
    String identifier;
    String description;
} RegDef_t;

typedef struct {
    Integer offset;
    String identifier;
    String name;
    String description;
} RegFieldDef_t;

typedef struct {
    Integer offs;
    function Bit#(dw) _(Bit#(aw) a) f_read;
} ReadOpPure_t#(numeric type dw);

typedef struct {
    Integer offs;
    function ActionValue#(Bit#(dw)) _(Bit#(aw) a) f_read;
} ReadOpImpure_t#(numeric type dw);

typedef union tagged {
    RegDef_t            RegDef;
    RegFieldDef_t       RegFieldDef;
    ReadOpPure_t#(dw)   ReadOpPure;
    ReadOpImpure_t#(dw) ReadOpImpure;
} RegMapEntry#(numeric type dw);

function List#(ReadOpPure_t#(dw))   get_pure_read       (RegMapEntry#(dw) regmap_entry) = regmap_entry matches tagged ReadOpPure    .rr ? Cons(rr, Nil) : Nil;
function List#(ReadOpImpure_t#(dw)) get_impure_read     (RegMapEntry#(dw) regmap_entry) = regmap_entry matches tagged ReadOpImpure  .rr ? Cons(rr, Nil) : Nil;
function List#(RegFieldDef_t)       get_regfield_def    (RegMapEntry#(dw) regmap_entry) = regmap_entry matches tagged RegFieldDef   .rr ? Cons(rr, Nil) : Nil;
function List#(RegDef_t)            get_reg_def         (RegMapEntry#(dw) regmap_entry) = regmap_entry matches tagged RegDef        .rr ? Cons(rr, Nil) : Nil;

typedef struct {
    String reg_defs;
} RegMapDoc_t#(numeric type dw);

typeclass FieldReadPure#(type src, numeric type dw);
    function Bit#(dw) field_read_pure(src s, Integer field_offs);
endtypeclass

instance FieldReadPure#(Reg#(t), dw) provisos(Bits#(t, st));
    function Bit#(dw) field_read_pure(Reg#(t) r, Integer field_offs);
        Bit#(dw) v = cExtend(r) << fromInteger(field_offs);
        return v;
    endfunction
endinstance

instance FieldReadPure#(t, dw) provisos(Bits#(t, st));
    function Bit#(dw) field_read_pure(t c, Integer field_offs);
        Bit#(dw) v = cExtend(c) << fromInteger(field_offs);
        return v;
    endfunction
endinstance

function ActionValue#(Bit#(dw)) field_read_impure(Reg#(t) r, Integer field_offs) 
    provisos(
        Bits#(t, st),
        FieldReadPure#(t, dw)
    );
    actionvalue
        return field_read_pure(r, field_offs);
    endactionvalue
endfunction

module [BlueBusCtx_t#(dw)] blue_reg_def#(Integer offs, String ident, String desc)();
    RegMapEntry#(dw) entry = tagged RegDef RegDef_t {
        offset:         offs,
        identifier:     ident,
        description:    desc
    };
    addToCollection(entry);
endmodule

module [BlueBusCtx_t#(dw)] blue_reg_co#(Integer offs, t v, Integer bitpos, String ident, String name, String desc)() 
    provisos(
        Bits#(t, sz_t),
        FieldReadPure#(t, dw)
    );
    function Bit#(dw) do_read(Bit#(aw) _a);
        return field_read_pure(v, bitpos);
    endfunction
    addToCollection(tagged ReadOpPure ReadOpPure_t { offs: offs, f_read: do_read });

    RegMapEntry#(dw) field_entry = tagged RegFieldDef RegFieldDef_t {
        offset:         offs,
        identifier:     ident,
        description:    desc
    };
    addToCollection(field_entry);
endmodule

module [BlueBusCtx_t#(dw)] blue_reg_rw#(Integer offs, t rv, Integer bitpos, String ident, String fname, String desc)(Reg#(t)) 
    provisos(
        Bits#(t, sz_t),
        FieldReadPure#(t, dw)
    );

    Reg#(t) r <- mkReg(rv);
    function Bit#(dw) do_read(Bit#(aw) _a);
        return field_read_pure(r, bitpos);
    endfunction
    addToCollection(tagged ReadOpPure ReadOpPure_t { offs: offs, f_read: do_read } );

    RegMapEntry#(dw) field_entry = tagged RegFieldDef RegFieldDef_t {
        offset:         offs,
        identifier:     ident,
        name:           fname,
        description:    desc
    };
    addToCollection(field_entry);

    return r;
endmodule

interface BusAccess_ifc#(type ext_ifc, type int_ifc);
    interface ext_ifc external;
    interface int_ifc internal;
endinterface

typedef BusAccess_ifc#(BlueBus_ifc#(aw, dw), int_ifc) BlueBusAccess_ifc#(numeric type aw, numeric type dw, type int_ifc);

module [Module] doc_blue_bus#(BlueBusCtx_t#(dw, i) ctx)(RegMapDoc_t#(dw));

    let {coll_device_ifc, c} <- getCollection(ctx);
    let regdefs     = List::concat(List::map(get_reg_def, c));
    let regfields   = List::concat(List::map(get_regfield_def, c));

    function String integerToHexDigitS(Integer n) = charToString(integerToHexDigit(n));

    function String integerToHex(Integer n);
        if (n < 16) return integerToHexDigitS(n);
        else return strConcat(integerToHex(n / 16), integerToHexDigitS(n % 16));
    endfunction

    function String doc_reg(RegDef_t regdef);
        return "" + integerToHex(regdef.offset) + " " + regdef.identifier + " " + regdef.description;
    endfunction

    String reg_doc = List::foldl(strConcat, "", List::map(strConcat("\n"), List::map(doc_reg, regdefs)));

    return RegMapDoc_t {
        reg_defs: reg_doc 
    };

endmodule

module [Module] create_blue_bus#(BlueBusCtx_t#(dw, i) ctx)(BlueBusAccess_ifc#(aw, dw, i));

    let {coll_device_ifc, c} <- getCollection(ctx);

    let pure_reads = List::concat(List::map(get_pure_read, c));

    function Bit#(dw) do_read_pure(Bit#(aw) addr);
        function Bit#(dw) fold_fn(Bit#(dw) acc, RegMapEntry#(dw) entry);
            case (entry) matches
                tagged ReadOpPure .op:
                    return acc | (fromInteger(op.offs) == addr ? op.f_read(addr) : 0);
                default: return acc;
            endcase
        endfunction
        return foldl(fold_fn, 0, c);
    endfunction

    interface BlueBus_ifc external;
        method Action write_strobed(Bit#(aw) addr, Bit#(dw) data, Bit#(TDiv#(dw, 8)) strobe);
            noAction;
        endmethod

        method Bit#(dw) read_pure(Bit#(aw) addr);
            return do_read_pure(addr);
        endmethod

    endinterface

    interface internal = coll_device_ifc;

endmodule



endpackage