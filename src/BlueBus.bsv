package BlueBus;

import List :: *;
import BUtils :: *;
import ModuleCollect :: *;
import Vector :: *;

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
    Integer bit_offset;
    Integer width;
    String reset_value;
} RegFieldDef_t;

typedef struct {
    Integer offs;
    function Bit#(dw) _(Bit#(aw) a) f_read;
} ReadOpPure_t#(numeric type dw);

typedef struct {
    Integer offs;
    function ActionValue#(Bit#(dw)) _(Bit#(aw) a) f_read;
} ReadOpImpure_t#(numeric type dw);

typedef struct {
    Integer offs;
    function Action _(Bit#(aw) a, Bit#(dw) d, Bit#(TDiv#(dw, 8)) s) f_write;
} WriteOp_t#(numeric type dw);

typedef union tagged {
    RegDef_t            RegDef;
    RegFieldDef_t       RegFieldDef;
    ReadOpPure_t#(dw)   ReadOpPure;
    ReadOpImpure_t#(dw) ReadOpImpure;
    WriteOp_t#(dw)      WriteOp;
} RegMapEntry#(numeric type dw);

function List#(ReadOpPure_t#(dw))   get_pure_read       (RegMapEntry#(dw) regmap_entry) = regmap_entry matches tagged ReadOpPure    .rr ? Cons(rr, Nil) : Nil;
function List#(ReadOpImpure_t#(dw)) get_impure_read     (RegMapEntry#(dw) regmap_entry) = regmap_entry matches tagged ReadOpImpure  .rr ? Cons(rr, Nil) : Nil;
function List#(WriteOp_t#(dw))      get_write_op        (RegMapEntry#(dw) regmap_entry) = regmap_entry matches tagged WriteOp      .rr ? Cons(rr, Nil) : Nil;
function List#(RegFieldDef_t)       get_regfield_def    (RegMapEntry#(dw) regmap_entry) = regmap_entry matches tagged RegFieldDef   .rr ? Cons(rr, Nil) : Nil;
function List#(RegDef_t)            get_reg_def         (RegMapEntry#(dw) regmap_entry) = regmap_entry matches tagged RegDef        .rr ? Cons(rr, Nil) : Nil;

typedef struct {
    String reg_defs;
} RegMapDoc_t#(numeric type dw);

interface BlueBusExport_ifc;
    method Bool done;
    method Bool success;
endinterface

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

function Integer bit_to_integer(Bit#(n) x);
    Integer res = 0;
    for (Integer i = 0; i < valueOf(n); i = i + 1)
        if (x[i] == 1)
            res = res + 2**i;
    return res;
endfunction

function Action field_write_strobed(Reg#(t) r, Integer field_offs, Bit#(dw) d, Bit#(b__) strobe)
    provisos(
        Bits#(t, st),
        Add#(st, a__, dw),
        Mul#(b__, 8, dw),
        Div#(dw, 8, b__)
    );
    action
        Bit#(dw) cur_word = cExtend(r) << fromInteger(field_offs);
        Vector#(TDiv#(dw, 8), Bit#(8)) cur_bytes = unpack(cur_word);
        Vector#(TDiv#(dw, 8), Bit#(8)) wr_bytes = unpack(d);
        for(Integer i = 0; i < valueOf(b__); i = i + 1) begin
            if(unpack(strobe[i])) begin
                cur_bytes[i] = wr_bytes[i];
            end
        end
        Bit#(dw) merged_word = pack(cur_bytes);
        Bit#(st) new_field = truncate(merged_word >> fromInteger(field_offs));
        r <= unpack(new_field);
    endaction
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
    function String integerToHexDigitS(Integer n) = charToString(integerToHexDigit(n));
    function String integerToHex(Integer n);
        if (n < 16) return integerToHexDigitS(n);
        else return strConcat(integerToHex(n / 16), integerToHexDigitS(n % 16));
    endfunction
    String reset_value = "0x" + integerToHex(bit_to_integer(pack(v)));
    addToCollection(tagged ReadOpPure ReadOpPure_t { offs: offs, f_read: do_read });

    RegMapEntry#(dw) field_entry = tagged RegFieldDef RegFieldDef_t {
        offset:         offs,
        identifier:     ident,
        name:           name,
        description:    desc,
        bit_offset:     bitpos,
        width:          valueOf(sz_t),
        reset_value:    reset_value
    };
    addToCollection(field_entry);
endmodule

module [BlueBusCtx_t#(dw)] blue_reg_rw#(Integer offs, t rv, Integer bitpos, String ident, String fname, String desc)(Reg#(t)) 
    provisos(
        Bits#(t, sz_t),
        FieldReadPure#(t, dw),
        Add#(sz_t, a__, dw),
        Mul#(TDiv#(dw, 8), 8, dw),
        Div#(dw, 8, TDiv#(dw, 8))
    );

    Reg#(t) r <- mkReg(rv);
    function String integerToHexDigitS(Integer n) = charToString(integerToHexDigit(n));
    function String integerToHex(Integer n);
        if (n < 16) return integerToHexDigitS(n);
        else return strConcat(integerToHex(n / 16), integerToHexDigitS(n % 16));
    endfunction
    String reset_value = "0x" + integerToHex(bit_to_integer(pack(rv)));

    function Bit#(dw) do_read(Bit#(aw) _a);
        return field_read_pure(r, bitpos);
    endfunction
    addToCollection(tagged ReadOpPure ReadOpPure_t { offs: offs, f_read: do_read } );

    function Action do_write(Bit#(aw) _a, Bit#(dw) d, Bit#(TDiv#(dw, 8)) s);
        action
            field_write_strobed(r, bitpos, d, s);
        endaction
    endfunction
    addToCollection(tagged WriteOp WriteOp_t { offs: offs, f_write: do_write });

    RegMapEntry#(dw) field_entry = tagged RegFieldDef RegFieldDef_t {
        offset:         offs,
        identifier:     ident,
        name:           fname,
        description:    desc,
        bit_offset:     bitpos,
        width:          valueOf(sz_t),
        reset_value:    reset_value
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
        String field_doc = "";
        for(Integer fi = 0; fi < length(regfields); fi = fi + 1) begin
            let rf = regfields[fi];
            if (rf.offset == regdef.offset) begin
                Integer msb = rf.bit_offset + rf.width - 1;
                field_doc = field_doc + "\n  " + rf.identifier + " [" + integerToString(msb) + ":" + integerToString(rf.bit_offset) + "] reset=" + rf.reset_value + " " + rf.description;
            end
        end
        return "" + integerToHex(regdef.offset) + " " + regdef.identifier + " " + regdef.description + field_doc;
    endfunction

    String reg_doc = List::foldl(strConcat, "", List::map(strConcat("\n"), List::map(doc_reg, regdefs)));

    return RegMapDoc_t {
        reg_defs: reg_doc 
    };

endmodule

module [Module] export_systemrdl_blue_bus#(BlueBusCtx_t#(dw, i) ctx, String output_path)(BlueBusExport_ifc);

    let {coll_device_ifc, c} <- getCollection(ctx);
    let regdefs         = List::concat(List::map(get_reg_def, c));
    let regfields       = List::concat(List::map(get_regfield_def, c));
    let pure_reads      = List::concat(List::map(get_pure_read, c));
    let impure_reads    = List::concat(List::map(get_impure_read, c));
    let writes          = List::concat(List::map(get_write_op, c));

    Reg#(Bool) rg_done <- mkReg(False);
    Reg#(Bool) rg_success <- mkReg(False);
    Reg#(Bool) rg_started <- mkReg(False);

    function String integerToHexDigitS(Integer n) = charToString(integerToHexDigit(n));

    function String integerToHex(Integer n);
        if (n < 16) return integerToHexDigitS(n);
        else return strConcat(integerToHex(n / 16), integerToHexDigitS(n % 16));
    endfunction

    function Bool has_read_access(Integer offs);
        Bool found = False;
        for(Integer i = 0; i < length(pure_reads); i = i + 1) begin
            found = found || pure_reads[i].offs == offs;
        end
        for(Integer i = 0; i < length(impure_reads); i = i + 1) begin
            found = found || impure_reads[i].offs == offs;
        end
        return found;
    endfunction

    function Bool has_write_access(Integer offs);
        Bool found = False;
        for(Integer i = 0; i < length(writes); i = i + 1) begin
            found = found || writes[i].offs == offs;
        end
        return found;
    endfunction

    function String get_sw(Bool has_read, Bool has_write);
        if (has_read && has_write) return "rw";
        else if (has_read) return "r";
        else if (has_write) return "w";
        else return "r";
    endfunction

    rule r_export_once (!rg_started);
        rg_started <= True;

        File fh <- $fopen(output_path, "w");
        if (fh == InvalidFile) begin
            $display("BlueBus SystemRDL export failed: could not open %s", output_path);
            rg_done <= True;
            rg_success <= False;
        end
        else begin
            $fwrite(fh, "// Auto-generated by export_systemrdl_blue_bus\n");
            $fwrite(fh, "// V1 note: field widths and bit offsets are inferred from BlueBus field declarations.\n");
            $fwrite(fh, "addrmap bluebus_export {\n");

            for(Integer ri = 0; ri < length(regdefs); ri = ri + 1) begin
                let rd = regdefs[ri];
                Bool can_read = has_read_access(rd.offset);
                Bool can_write = has_write_access(rd.offset);
                String sw = get_sw(can_read, can_write);
                Bool wrote_field = False;

                $fwrite(fh, "  reg {\n");
                $fwrite(fh, "    desc = \"%s\";\n", rd.description);

                for(Integer fi = 0; fi < length(regfields); fi = fi + 1) begin
                    let rf = regfields[fi];
                    if (rf.offset == rd.offset) begin
                        Integer msb = rf.bit_offset + rf.width - 1;
                        $fwrite(fh, "    field { sw = %s; desc = \"%s\"; } %s[%0d:%0d] = %s;\n", sw, rf.description, rf.identifier, msb, rf.bit_offset, rf.reset_value);
                        wrote_field = True;
                    end
                end

                if (!wrote_field) begin
                    $fwrite(fh, "    field { sw = %s; desc = \"No field metadata\"; } RESERVED[0:0];\n", sw);
                end

                $fwrite(fh, "  } %s @ 0x%s;\n", rd.identifier, integerToHex(rd.offset));
            end

            $fwrite(fh, "};\n");
            $fflush(fh);
            $fclose(fh);
            rg_done <= True;
            rg_success <= True;
            $display("BlueBus SystemRDL export complete: %s", output_path);
        end
    endrule

    method Bool done;
        return rg_done;
    endmethod

    method Bool success;
        return rg_success;
    endmethod

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
        return List::foldl(fold_fn, 0, c);
    endfunction

    function Action do_write_strobed(Bit#(aw) addr, Bit#(dw) data, Bit#(TDiv#(dw, 8)) strobe);
        function Action fold_write_fn(Action acc, RegMapEntry#(dw) entry);
            case (entry) matches
                tagged WriteOp .op:
                    return action
                        acc;
                        if(fromInteger(op.offs) == addr) begin
                            op.f_write(addr, data, strobe);
                        end
                    endaction;
                default:
                    return acc;
            endcase
        endfunction
        return List::foldl(fold_write_fn, noAction, c);
    endfunction

    interface BlueBus_ifc external;
    
        method Action write_strobed(Bit#(aw) addr, Bit#(dw) data, Bit#(TDiv#(dw, 8)) strobe);
            do_write_strobed(addr, data, strobe);
        endmethod

        method Bit#(dw) read_pure(Bit#(aw) addr);
            return do_read_pure(addr);
        endmethod

    endinterface

    interface internal = coll_device_ifc;

endmodule



endpackage