(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type gc_action =
  | Must_scan
  | Can_scan
  | Cannot_scan
  | Cannot_be_live_at_gc

type machtype_component =
  | Int_reg of gc_action
  | Float_reg

type machtype = machtype_component array

let typ_void = ([||] : machtype_component array)
let typ_val = [| Int_reg Must_scan |]
let typ_derived = [| Int_reg Cannot_be_live_at_gc |]
let typ_int = [| Int_reg Can_scan |]
let typ_float = [| Float_reg |]
let typ_raw = [| Int_reg Cannot_scan |]

(* [gc_actions] are partially ordered as follows:

      Cannot_be_live_at_gc
              ^
            /   \
           /     \
          /       \
         /         \
        /           \
    Must_scan   Cannot_scan
        ^           ^
         \          /
          \        /
           \      /
            \    /
           Can_scan

  In particular, [Cannot_be_live_at_gc] must be above [Must_scan], to ensure
  that if there is a join point between a code path yielding
  [Cannot_scan] and one yielding [Must_scan] then the result is treated
  as a derived pointer into the heap (i.e. [Cannot_be_live_at_gc]). (Such a
  result may not be live across any call site or a fatal compiler error will
  result.)
*)

let lub_gc_action act1 act2 =
  match act1, act2 with
  | Can_scan, Can_scan -> Can_scan
  | Can_scan, Must_scan -> Must_scan
  | Can_scan, Cannot_be_live_at_gc -> Cannot_be_live_at_gc
  | Can_scan, Cannot_scan -> Cannot_scan
  | Must_scan, Can_scan -> Must_scan
  | Must_scan, Must_scan -> Must_scan
  | Must_scan, Cannot_be_live_at_gc -> Cannot_be_live_at_gc
  | Must_scan, Cannot_scan -> assert false
  | Cannot_be_live_at_gc, Can_scan -> Cannot_be_live_at_gc
  | Cannot_be_live_at_gc, Must_scan -> Cannot_be_live_at_gc
  | Cannot_be_live_at_gc, Cannot_be_live_at_gc -> Cannot_be_live_at_gc
  | Cannot_be_live_at_gc, Cannot_scan -> Cannot_be_live_at_gc
  | Cannot_scan, Can_scan -> Cannot_scan
  | Cannot_scan, Must_scan -> assert false
  | Cannot_scan, Cannot_be_live_at_gc -> Cannot_be_live_at_gc
  | Cannot_scan, Cannot_scan -> Cannot_scan

let lub_component comp1 comp2 =
  match comp1, comp2 with
  | Int_reg act1, Int_reg act2 -> Int_reg (lub_gc_action act1 act2)
  | Float_reg, Float_reg -> Float_reg
  | Int_reg _, Float_reg
  | Float_reg, Int_reg _ ->
    (* Float unboxing code must be sure to avoid this case. *)
    assert false

let ge_gc_action act1 act2 =
  match act1, act2 with
  | Can_scan, Can_scan -> true
  | Can_scan, Must_scan -> false
  | Can_scan, Cannot_be_live_at_gc -> false
  | Can_scan, Cannot_scan -> false
  | Must_scan, Can_scan -> true
  | Must_scan, Must_scan -> true
  | Must_scan, Cannot_be_live_at_gc -> false
  | Must_scan, Cannot_scan -> assert false
  | Cannot_be_live_at_gc, Can_scan -> true
  | Cannot_be_live_at_gc, Must_scan -> true
  | Cannot_be_live_at_gc, Cannot_be_live_at_gc -> true
  | Cannot_be_live_at_gc, Cannot_scan -> true
  | Cannot_scan, Can_scan -> true
  | Cannot_scan, Must_scan -> assert false
  | Cannot_scan, Cannot_be_live_at_gc -> false
  | Cannot_scan, Cannot_scan -> true

let ge_component comp1 comp2 =
  match comp1, comp2 with
  | Int_reg act1, Int_reg act2 -> ge_gc_action act1 act2
  | Float_reg, Float_reg -> true
  | Int_reg _, Float_reg
  | Float_reg, Int_reg _ -> assert false

type integer_comparison = Lambda.integer_comparison =
  | Ceq | Cne | Clt | Cgt | Cle | Cge

let negate_integer_comparison = Lambda.negate_integer_comparison

let swap_integer_comparison = Lambda.swap_integer_comparison

(* With floats [not (x < y)] is not the same as [x >= y] due to NaNs,
   so we provide additional comparisons to represent the negations.*)
type float_comparison = Lambda.float_comparison =
  | CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

let negate_float_comparison = Lambda.negate_float_comparison

let swap_float_comparison = Lambda.swap_float_comparison
type label = int

let label_counter = ref 99

let new_label() = incr label_counter; !label_counter

type rec_flag = Nonrecursive | Recursive

type phantom_defining_expr =
  | Cphantom_const_int of Targetint.t
  | Cphantom_const_symbol of string
  | Cphantom_var of Backend_var.t
  | Cphantom_offset_var of { var : Backend_var.t; offset_in_words : int; }
  | Cphantom_read_field of { var : Backend_var.t; field : int; }
  | Cphantom_read_symbol_field of { sym : string; field : int; }
  | Cphantom_block of { tag : int; fields : Backend_var.t list; }

type memory_chunk =
    Byte_unsigned
  | Byte_signed
  | Sixteen_unsigned
  | Sixteen_signed
  | Thirtytwo_unsigned
  | Thirtytwo_signed
  | Word of gc_action
  | Single
  | Double
  | Double_u

type symbol_kind =
  | Function
  | Value
  | Other

let machtype_component_of_symbol_kind kind =
  match kind with
  | Value -> Int_reg Can_scan
  | Function | Other -> Int_reg Cannot_scan

type operation =
    Capply of machtype
  | Cextcall of string * machtype * bool * label option
    (** If specified, the given label will be placed immediately after the
        call (at the same place as any frame descriptor would reference). *)
  | Cload of memory_chunk * Asttypes.mutable_flag
  | Calloc
  | Cstore of memory_chunk * Lambda.initialization_or_assignment
  | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Ccmpi of integer_comparison
  | Caddv | Cadda
  | Ccmpa of integer_comparison
  | Cnegf | Cabsf
  | Caddf | Csubf | Cmulf | Cdivf
  | Cfloatofint | Cintoffloat
  | Ccmpf of float_comparison
  | Craise of Lambda.raise_kind
  | Ccheckbound

type expression =
    Cconst_int of int * Debuginfo.t
  | Cconst_natint of nativeint * Debuginfo.t
  | Cconst_float of float * Debuginfo.t
  | Cconst_symbol of string * symbol_kind * Debuginfo.t
  | Cconst_pointer of int * Debuginfo.t
  | Cconst_natpointer of nativeint * Debuginfo.t
  | Cblockheader of nativeint * Debuginfo.t
  | Cvar of Backend_var.t
  | Clet of Backend_var.With_provenance.t * expression * expression
  | Cphantom_let of Backend_var.With_provenance.t
      * phantom_defining_expr option * expression
  | Cassign of Backend_var.t * expression
  | Ctuple of expression list
  | Cop of operation * expression list * Debuginfo.t
  | Csequence of expression * expression
  | Cifthenelse of expression * Debuginfo.t * expression
      * Debuginfo.t * expression * Debuginfo.t
  | Cswitch of expression * int array * (expression * Debuginfo.t) array
      * Debuginfo.t
  | Ccatch of
      rec_flag
        * (int * (Backend_var.With_provenance.t * machtype) list
          * expression * Debuginfo.t) list
        * expression
  | Cexit of int * expression list
  (* CR Gbury: There is no check that the machtype of
               catches and exit match. *)
  | Ctrywith of expression * Backend_var.With_provenance.t * expression
      * Debuginfo.t

type codegen_option =
  | Reduce_code_size
  | No_CSE

type fundecl =
  { fun_name: string;
    fun_args: (Backend_var.With_provenance.t * machtype) list;
    fun_body: expression;
    fun_codegen_options : codegen_option list;
    fun_dbg : Debuginfo.t;
  }

type data_item =
    Cdefine_symbol of string
  | Cglobal_symbol of string
  | Cint8 of int
  | Cint16 of int
  | Cint32 of nativeint
  | Cint of nativeint
  | Csingle of float
  | Cdouble of float
  | Csymbol_address of string
  | Cstring of string
  | Cskip of int
  | Calign of int

type phrase =
    Cfunction of fundecl
  | Cdata of data_item list

let ccatch (i, ids, e1, e2, dbg) =
  Ccatch(Nonrecursive, [i, ids, e2, dbg], e1)

let reset () =
  label_counter := 99

let iter_shallow_tail f = function
  | Clet(_, _, body) | Cphantom_let (_, _, body) ->
      f body;
      true
  | Cifthenelse(_cond, _ifso_dbg, ifso, _ifnot_dbg, ifnot, _dbg) ->
      f ifso;
      f ifnot;
      true
  | Csequence(_e1, e2) ->
      f e2;
      true
  | Cswitch(_e, _tbl, el, _dbg') ->
      Array.iter (fun (e, _dbg) -> f e) el;
      true
  | Ccatch(_rec_flag, handlers, body) ->
      List.iter (fun (_, _, h, _dbg) -> f h) handlers;
      f body;
      true
  | Ctrywith(e1, _id, e2, _dbg) ->
      f e1;
      f e2;
      true
  | Cexit _ | Cop (Craise _, _, _) ->
      true
  | Cconst_int _
  | Cconst_natint _
  | Cconst_float _
  | Cconst_symbol _
  | Cconst_pointer _
  | Cconst_natpointer _
  | Cblockheader _
  | Cvar _
  | Cassign _
  | Ctuple _
  | Cop _ ->
      false

let rec map_tail f = function
  | Clet(id, exp, body) ->
      Clet(id, exp, map_tail f body)
  | Cphantom_let(id, exp, body) ->
      Cphantom_let (id, exp, map_tail f body)
  | Cifthenelse(cond, ifso_dbg, ifso, ifnot_dbg, ifnot, dbg) ->
      Cifthenelse
        (
          cond,
          ifso_dbg, map_tail f ifso,
          ifnot_dbg, map_tail f ifnot,
          dbg
        )
  | Csequence(e1, e2) ->
      Csequence(e1, map_tail f e2)
  | Cswitch(e, tbl, el, dbg') ->
      Cswitch(e, tbl, Array.map (fun (e, dbg) -> map_tail f e, dbg) el, dbg')
  | Ccatch(rec_flag, handlers, body) ->
      let map_h (n, ids, handler, dbg) = (n, ids, map_tail f handler, dbg) in
      Ccatch(rec_flag, List.map map_h handlers, map_tail f body)
  | Ctrywith(e1, id, e2, dbg) ->
      Ctrywith(map_tail f e1, id, map_tail f e2, dbg)
  | Cexit _ | Cop (Craise _, _, _) as cmm ->
      cmm
  | Cconst_int _
  | Cconst_natint _
  | Cconst_float _
  | Cconst_symbol _
  | Cconst_pointer _
  | Cconst_natpointer _
  | Cblockheader _
  | Cvar _
  | Cassign _
  | Ctuple _
  | Cop _ as c ->
      f c

let map_shallow f = function
  | Clet (id, e1, e2) ->
      Clet (id, f e1, f e2)
  | Cphantom_let (id, de, e) ->
      Cphantom_let (id, de, f e)
  | Cassign (id, e) ->
      Cassign (id, f e)
  | Ctuple el ->
      Ctuple (List.map f el)
  | Cop (op, el, dbg) ->
      Cop (op, List.map f el, dbg)
  | Csequence (e1, e2) ->
      Csequence (f e1, f e2)
  | Cifthenelse(cond, ifso_dbg, ifso, ifnot_dbg, ifnot, dbg) ->
      Cifthenelse(f cond, ifso_dbg, f ifso, ifnot_dbg, f ifnot, dbg)
  | Cswitch (e, ia, ea, dbg) ->
      Cswitch (e, ia, Array.map (fun (e, dbg) -> f e, dbg) ea, dbg)
  | Ccatch (rf, hl, body) ->
      let map_h (n, ids, handler, dbg) = (n, ids, f handler, dbg) in
      Ccatch (rf, List.map map_h hl, f body)
  | Cexit (n, el) ->
      Cexit (n, List.map f el)
  | Ctrywith (e1, id, e2, dbg) ->
      Ctrywith (f e1, id, f e2, dbg)
  | Cconst_int _
  | Cconst_natint _
  | Cconst_float _
  | Cconst_symbol _
  | Cconst_pointer _
  | Cconst_natpointer _
  | Cblockheader _
  | Cvar _
    as c ->
      c
