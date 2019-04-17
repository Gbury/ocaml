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

(* Second intermediate language (machine independent) *)

(** The semantics of the GC upon the contents of a register. *)
type gc_action =
  | Must_scan
    (** The contents of the register must be scanned and if needs be updated
        by the GC.  (That is to say, the register will become a GC root.) *)
  | Can_scan
    (** The contents of the register do not have to be scanned or updated by
        the GC.  However it is permissible for it to do so.  (A case where
        a scan can happen is when there is a conditional with one arm returning
        [Must_scan] and the other arm returning [Can_scan].  The resulting
        register will be marked as [Must_scan].)
        Two examples of values in registers that might be marked [Can_scan]:
        1. A tagged OCaml integer.
        2. A pointer to a block living outside of the OCaml heap (which, itself
           and transitively, does not point back into the heap) but whose
           structure reflects that of a valid OCaml value. *)
  | Cannot_scan
    (** The contents of the register must not be scanned by the GC (for
        example it may contain a code pointer, or a pointer to arbitrary data
        outside the OCaml heap).  However, it is permissible for the register
        to be live across a GC. *)
  | Cannot_be_live_at_gc
    (** As for [Cannot_scan] with the additional restriction that the
        register cannot be live when the GC is called.  This is used for
        registers holding pointers derived from the addresses of blocks in
        the heap (for example when indexing into an array; the resulting
        derived pointer points into the middle of a block).  The values of
        such derived pointers may change when the GC is invoked. *)

(** Types of registers.  These guide register allocation and determine how
    local variables are tracked by the GC. *)
type machtype_component =
  | Int_reg of gc_action
  (** The value is held in an integer register; the GC behaves as given
      by the [gc_action]. *)
  | Float_reg
  (** The value, an unboxed float, is held in a floating-point register.
      The GC never scans or updates such registers. *)

type machtype = machtype_component array

val typ_void: machtype

val typ_val: machtype
(** Register type for holding arbitrary OCaml values. *)

val typ_derived: machtype
(** Register type for holding derived pointers into the OCaml heap. *)

val typ_int: machtype
(** Register type for holding tagged OCaml integers. *)

val typ_raw : machtype
(** Register type for holding arbitrary (untagged) machine integers. *)

val typ_float: machtype
(** Register type for holding unboxed floating-point numbers. *)

val size_component: machtype_component -> int

(* Size of machine integers manipulated by arithmetic operations *)
type size_arith =
  | A32
  | A64
  | Atarget

(** Least upper bound of two [machtype_component]s. *)
val lub_component
   : machtype_component
  -> machtype_component
  -> machtype_component

(** Returns [true] iff the first supplied [machtype_component] is greater than
    or equal to the second under the relation used by [lub_component]. *)
val ge_component
   : machtype_component
  -> machtype_component
  -> bool

val size_machtype: machtype -> int

type integer_comparison = Lambda.integer_comparison =
  | Ceq | Cne | Clt | Cgt | Cle | Cge

val negate_integer_comparison: integer_comparison -> integer_comparison
val swap_integer_comparison: integer_comparison -> integer_comparison

type float_comparison = Lambda.float_comparison =
  | CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

val negate_float_comparison: float_comparison -> float_comparison
val swap_float_comparison: float_comparison -> float_comparison

type label = int
val new_label: unit -> label

type raise_kind =
  | Raise_withtrace
  | Raise_notrace

type rec_flag = Nonrecursive | Recursive

type phantom_defining_expr =
  (* CR-soon mshinwell: Convert this to [Targetint.OCaml.t] (or whatever the
     representation of "target-width OCaml integers of type [int]"
     becomes when merged). *)
  | Cphantom_const_int of Targetint.t
  (** The phantom-let-bound variable is a constant integer.
      The argument must be the tagged representation of an integer within
      the range of type [int] on the target.  (Analogously to [Cconst_int].) *)
  | Cphantom_const_symbol of string
  (** The phantom-let-bound variable is an alias for a symbol. *)
  | Cphantom_var of Backend_var.t
  (** The phantom-let-bound variable is an alias for another variable.  The
      aliased variable must not be a bound by a phantom let. *)
  | Cphantom_offset_var of { var : Backend_var.t; offset_in_words : int; }
  (** The phantom-let-bound-variable's value is defined by adding the given
      number of words to the pointer contained in the given identifier. *)
  | Cphantom_read_field of { var : Backend_var.t; field : int; }
  (** The phantom-let-bound-variable's value is found by adding the given
      number of words to the pointer contained in the given identifier, then
      dereferencing. *)
  | Cphantom_read_symbol_field of { sym : string; field : int; }
  (** As for [Uphantom_read_var_field], but with the pointer specified by
      a symbol. *)
  | Cphantom_block of { tag : int; fields : Backend_var.t list; }
  (** The phantom-let-bound variable points at a block with the given
      structure. *)

type symbol_type =
  | Function
  (** A code pointer. *)
  | Value
  (** An out-of-heap value that may be scanned (but of course does not need
      to be scanned) by the GC. *)
  | Other
  (** An out-of-heap value that cannot be scanned by the GC. *)

type memory_chunk =
    Byte_unsigned
  | Byte_signed
  | Sixteen_unsigned
  | Sixteen_signed
  | Thirtytwo_unsigned
  | Thirtytwo_signed
  | Word of gc_action
  | Single
  | Double                             (* 64-bit-aligned 64-bit float *)
  | Double_u                           (* word-aligned 64-bit float *)

(** Types of pointers to statically-allocated code and data. *)
type symbol_kind =
  | Function
  (** A pointer to the code of a function. *)
  | Value
  (** A pointer to a block outside the OCaml heap, which does not (itself and
      transitively) point back into the heap, which has the correct structure
      for an OCaml value.  Such blocks should be marked black to avoid wasting
      GC time.  (They must be marked black if in a read-only section.) *)
  | Other
  (** A pointer to arbitrary out-of-heap data. *)

val machtype_component_of_symbol_kind : symbol_kind -> machtype_component
(** The register type required to hold the address of a symbol with the
    given kind. *)

type operation =
    Capply of machtype
  | Cextcall of string * machtype * bool * label option
  | Cload of memory_chunk * Asttypes.mutable_flag
  | Calloc
  | Cstore of memory_chunk * Lambda.initialization_or_assignment
  | Cadd of size_arith * gc_action  (* the [gc_action] gives the type of the result *)
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
  | Ccmps of size_arith * integer_comparison  (* signed integer comparison *)
  | Ccmpu of size_arith * integer_comparison  (* unsigned integer comparison *)
  | Cnegf | Cabsf
  | Caddf | Csubf | Cmulf | Cdivf
  | Cfloatofint | Cintoffloat
  | Ccmpf of float_comparison
  | Craise of raise_kind
  | Ccheckbound

(** Not all cmm expressions currently have [Debuginfo.t] values attached to
    them.  The ones that do are those that are likely to generate code that
    can fairly robustly be mapped back to a source location.  In the future
    it might be the case that more [Debuginfo.t] annotations are desirable. *)
and expression =
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

val ccatch :
     int * (Backend_var.With_provenance.t * machtype) list
       * expression * expression
  -> expression

val reset : unit -> unit
