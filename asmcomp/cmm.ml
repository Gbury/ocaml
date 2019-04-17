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

let () =
  assert (Arch.size_int = Arch.size_addr)

let size_component = function
  | Int_reg _ -> Arch.size_addr
  | Float_reg -> Arch.size_float

type size_arith =
  | A32
  | A64
  | Atarget

(* [gc_actions] are partially ordered as follows:

            Cannot_be_live_at_gc
                     ^
                     |
                     |
                     |
                     |
                     |
    Must_scan   Cannot_scan
        ^            ^
         \          /
          \        /
           \      /
            \    /
           Can_scan

*)

let lub_gc_action act1 act2 =
  match act1, act2 with
  | Can_scan, Can_scan -> Can_scan
  | Can_scan, Must_scan -> Must_scan
  | Can_scan, Cannot_be_live_at_gc -> Cannot_be_live_at_gc
  | Can_scan, Cannot_scan -> Cannot_scan
  | Must_scan, Can_scan -> Must_scan
  | Must_scan, Must_scan -> Must_scan
  | Must_scan, Cannot_be_live_at_gc -> assert false
  | Must_scan, Cannot_scan -> assert false
  | Cannot_be_live_at_gc, Can_scan -> Cannot_be_live_at_gc
  | Cannot_be_live_at_gc, Must_scan -> assert false
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
  | Must_scan, Cannot_be_live_at_gc -> assert false
  | Must_scan, Cannot_scan -> assert false
  | Cannot_be_live_at_gc, Can_scan -> true
  | Cannot_be_live_at_gc, Must_scan -> assert false
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

let size_machtype mty =
  let size = ref 0 in
  for i = 0 to Array.length mty - 1 do
    size := !size + size_component mty.(i)
  done;
  !size

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

type raise_kind =
  | Raise_withtrace
  | Raise_notrace

type rec_flag = Nonrecursive | Recursive

type phantom_defining_expr =
  | Cphantom_const_int of Targetint.t
  | Cphantom_const_symbol of string
  | Cphantom_var of Backend_var.t
  | Cphantom_offset_var of { var : Backend_var.t; offset_in_words : int; }
  | Cphantom_read_field of { var : Backend_var.t; field : int; }
  | Cphantom_read_symbol_field of { sym : string; field : int; }
  | Cphantom_block of { tag : int; fields : Backend_var.t list; }

type symbol_type =
  | Function
  | Value
  | Other

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
  | Cadd of size_arith * gc_action
  | Csub of size_arith * gc_action
  | Cmul of size_arith * gc_action
  | Cmulh of size_arith * gc_action
  | Cdiv of size_arith * gc_action
  | Cmod of size_arith * gc_action
  | Cand of size_arith * gc_action
  | Cor of size_arith * gc_action
  | Cxor of size_arith * gc_action
  | Clsl of size_arith * gc_action
  | Clsr of size_arith * gc_action
  | Casr of size_arith * gc_action
  | Ccmps of size_arith * integer_comparison
  | Ccmpu of size_arith * integer_comparison
  | Cnegf | Cabsf
  | Caddf | Csubf | Cmulf | Cdivf
  | Cfloatofint | Cintoffloat
  | Ccmpf of float_comparison
  | Craise of raise_kind
  | Ccheckbound

type expression =
    Cconst_int of int
  | Cconst_natint of nativeint
  | Cconst_float of float
  | Cconst_symbol of string * symbol_kind
  | Cconst_pointer of int
  | Cconst_natpointer of nativeint
  | Cblockheader of nativeint * Debuginfo.t
  | Cvar of Backend_var.t
  | Clet of Backend_var.With_provenance.t * expression * expression
  | Cphantom_let of Backend_var.With_provenance.t
      * phantom_defining_expr option * expression
  | Cassign of Backend_var.t * expression
  | Ctuple of expression list
  | Cop of operation * expression list * Debuginfo.t
  | Csequence of expression * expression
  | Cifthenelse of expression * expression * expression
  | Cswitch of expression * int array * expression array * Debuginfo.t
  | Ccatch of
      rec_flag
        * (int * (Backend_var.With_provenance.t * machtype) list
          * expression) list
        * expression
  | Cexit of int * expression list
  (* CR Gbury: There is no check that the machtype of
               catches and exit match. *)
  | Ctrywith of expression * Backend_var.With_provenance.t * expression

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

let ccatch (i, ids, e1, e2)=
  Ccatch(Nonrecursive, [i, ids, e2], e1)

let reset () =
  label_counter := 99
