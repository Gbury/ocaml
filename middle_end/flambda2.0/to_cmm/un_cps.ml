(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import

(* Notes:
   - an int64 on a 32-bit host is represented across two registers,
     hence most operations on them will actually need to call C primitive
      that can handle them.
   - int32 on 64 bits are represented as an int64 in the range of
     32-bit integers. Currently we trust flambda2 to insert
     double shifts to clear the higher order 32-bits between operations.
     Once the samll_arith PR comes, we can use dedicated 32-bits
     cmm arithmetic operations.
*)

(* TODO: remove all uses of this, ^^ *)
let todo () = failwith "Not yet implemented"

(* Are we compiling on/for a 32-bit architecture ? *)
let arch32 = Arch.size_int = 32

let typ_int = Cmm.typ_int
let typ_val = Cmm.typ_val
let typ_float = Cmm.typ_float

let typ_int64 =
  if arch32 then [| Cmm.Int; Cmm.Int |] else [| Cmm.Int |]

(* Translation environment *)

module Env = struct
  type t = {
    cont  : Continuation.t;
    (* The continuation of the current context
       (used to determine which calls are tail-calls) *)
    exn   : Continuation.t;
    (* The exception continuation of the current context
       (used to determine where to insert try-with blocks) *)
    vars  : Backend_var.t Variable.Map.t;
    (* Map from flambda2 variables to backend_variables *)
  }

  (* Variables *)

  let create_variable env v =
    assert (not (Variable.Map.mem v env.vars));
    let name = Variable.unique_name v in
    let v' = Backend_var.create_local name in
    let vars = Variable.Map.add v v' env.vars in
    { env with vars }, v'

  let get_variable env v =
    try Variable.Map.find v env.vars
    with Not_found -> assert false

end


(* Cmm helpers *)

module C = struct

  include Cmm_helpers

  (* Constructors for constants *)

  let var v = Cmm.Cvar v

  let symbol ?(dbg=Debuginfo.none) s =
    Cmm.Cconst_symbol (s, dbg)

  let float ?(dbg=Debuginfo.none) f =
    Cmm.Cconst_float (f, dbg)

  (* CR Gbury: this conversion int -> nativeint is potentially unsafe
     when cross-compiling for 64-bit on a 32-bit host *)
  let int ?(dbg=Debuginfo.none) i =
    natint_const_untagged dbg (Nativeint.of_int i)

  let int32 ?(dbg=Debuginfo.none) i =
    natint_const_untagged dbg (Nativeint.of_int32 i)

  (* CR Gbury: this conversion int64 -> nativeint is potentially unsafe
       when cross-compiling for 64-bit on a 32-bit host *)
  let int64 ?(dbg=Debuginfo.none) i =
    natint_const_untagged dbg (Int64.to_nativeint i)

  let targetint ?(dbg=Debuginfo.none) t =
    match Targetint.repr t with
    | Int32 i -> int32 ~dbg i
    | Int64 i -> int64 ~dbg i

  (* Boxing/unboxing *)

  let primitive_boxed_int_of_boxable_number = function
    | Flambda_kind.Boxable_number.Naked_float -> assert false
    | Flambda_kind.Boxable_number.Naked_int32 -> Primitive.Pint32
    | Flambda_kind.Boxable_number.Naked_int64 -> Primitive.Pint64
    | Flambda_kind.Boxable_number.Naked_nativeint -> Primitive.Pnativeint

  let unbox_number ?(dbg=Debuginfo.none) kind arg =
    match kind with
    | Flambda_kind.Boxable_number.Naked_float -> C.unbox_float dbg arg
    | _ ->
        let primitive_kind = primitive_unboxed_int_of_boxable_number kind in
        unbox_int primitive_kind arg dbg

  let box_number ?(dbg=Debuginfo.none) kind arg =
    match kind with
    | Flambda_kind.Boxable_number.Naked_float -> C.box_float dbg arg
    | _ ->
        let primitive_kind = primitive_unboxed_int_of_boxable_number kind in
        box_int_gen dbg primitive_kind arg

  let box_int64 ?dbg arg =
    box_number ?dbg Flambda_kind.Boxable_number.Naked_int64 arg


  (* Constructors for operations *)

  let unary op = (fun ?(dbg=Debuginfo.none) x -> Cmm.Cop (op, [x], dbg))
  let binary op = (fun ?(dbg=Debuginfo.none) x y -> Cmm.Cop (op, [a; b], dbg))

  let eq = binary Cmm.(Ccmpi Ceq)
  let neq = binary Cmm.(Ccmpi Cne)

  let lt = binary Cmm.(Ccmpi Clt)
  let le = binary Cmm.(Ccmpi Cle)
  let gt = binary Cmm.(Ccmpi Cgt)
  let ge = binary Cmm.(Ccmpi Cgt)

  let ult = binary Cmm.(Ccmpa Clt)
  let ule = binary Cmm.(Ccmpa Cle)
  let ugt = binary Cmm.(Ccmpa Cgt)
  let uge = binary Cmm.(Ccmpa Cgt)

  let float_abs = unary Cmm.Cabsf
  let float_neg = unary Cmm.Cnegf

  let float_add = binary Cmm.Caddf
  let float_sub = binary Cmm.Csubf
  let float_mul = binary Cmm.Cmulf
  let float_div = binary Cmm.Cdivf

  let float_eq = binary Cmm.CFeq
  let float_lt = binary Cmm.CFlt
  let float_le = binary Cmm.CFle
  let float_gt = binary Cmm.CFgt
  let float_ge = binary Cmm.CFge

  let int_of_float = unary Cmm.Cintoffloat
  let float_of_int = unary Cmm.Cfloatofint

  let letin v e body = Cmm.Clet (v, e, body)

  let ite
      ?(dbg=Debuginfo.none) ?(then_dbg=Debuginfo.none) ?(else_dbg=Debuginfo.none)
      ~then_ ~else_ cond =
    Cifthenelse(cond, dbg, then_, then_dbg, else_, else_dbg)

  let load ?(dbg=Debuginfo.none) kind mut addr =
    Cmm.Cop (Cmm.Cload (kind, mut), [addr], dbg)

  let store ?(dbg=Debuginfo.none) kind init addr value =
    Cmm.Cop (Cmm.Cstore (kind, init), [addr; value], dbg)

  let extcall ?(dbg=Debuginfo.none) ?label ~alloc name typ_res args =
    Cmm.Cop (Cextcall (name, typ_res, alloc, label), args, dbg)

  (* Block creation *)

  let float_tag = function
    | [] -> assert false
    | [_] -> Tag.double_tag
    | _ -> Tag.double_array_tag

  let make_block ?(dbg=Debuginfo.none) kind mut args =
    match kind with
    | Flambda_primitive.Full_of_values (tag, _) ->
        make_alloc dbg (Tag.to_int tag) args
    | Flambda_primitive.Full_of_naked_floats
    | Flambda_primitive.Generic_array
        Flambda_primitive.Generic_array_specialisation.Full_of_naked_floats ->
        make_float_alloc dbg (float_tag args) args
    | _ ->
        make_alloc dbg 0 args

  (* Block access *)

  let array_kind_of_block_access kind =
    let open Flambda_primitive in
    match kind with
    (* Full naked float arrays *)
    | Block_access_kind.(Block Naked_float)
    | Block_access_kind.(Array Naked_float)
    | Block_access_kind.Generic_array
        Generic_array_specialisation.Full_of_naked_floats ->
        Lambda.Pfloatarray
    (* Arrays of caml values (i.e specifically not naked floats) *)
    | Block_access_kind.Generic_array
        Generic_array_specialisation.Full_of_arbitrary_values_but_not_float ->
        Lambda.Paddrarray
    (* Arrays (or accesses) to immediate integers *)
    | Block_access_kind.(Block Value Value_kind.Definitely_immediate)
    | Block_access_kind.(Array Value Value_kind.Definitely_immediate)
    | Block_access_kind.Generic_array
        Generic_array_specialisation.Full_of_immediates ->
        Lambda.Pintarray
    (* General case: the array might contain naked floats *)
    | _ -> Lambda.Pgenarray

  let block_length ?(dbg=Debuginfo.none) block_access_kind block =
    arraylength (array_kind_of_block_access block_acess_kind) block dbg

  let block_load ?(dbg=Debuginfo.none) kind block index =
    match array_kind_of_block_access kind with
    | Lambda.Pintarray -> int_array_ref block index dbg
    | Lambda.Paddrarray -> addr_array_ref block index dbg
    | Lambda.Pfloatarray -> unboxed_float_array_ref block index dbg
    | Lambda.Pgenarray ->
        ite ~dbg (is_addr_array_ptr block dbg)
          ~then_:(addr_array_ref block index dbg) ~then_dbg:dbg
          ~else_:(float_array_ref block index dbg) ~else_dbg:dbg

  let block_set ?(dbg=Debuginfo.none) kind init block index value =
    match kind, init with
    | Lambda.Pintarray, Flambda_primitive.Assignment ->
        set_field block index value Lambda.Assignment
    | Lambda.Pintarray, Flambda_primitive.Initialization ->
        set_field block index value Lambda.Heap_initialization
    | Lambda.Paddrarray, Flambda_primitive.Assignment ->
        return_unit dbg (addr_array_set block index value dbg)
    | Lambda.Paddrarray, Flambda_primitive.Initialization ->
        return_unit dbg (addr_array_initialize block index value dbg)
    | Lambda.Pfloatarray, Flambda_primitive.(Assignment|Initialization) ->
        return_unit dbg (float_array_set block index value dbg)
    | Lambda.Pgenarray, Flambda_primitive.(Assignment|Intialization) ->
        return_unit dbg (
          ite ~dbg (is_addr_array_ptr block dbg)
            ~then_:(add_array_set block index value dbg) ~then_dbg:dbg
            ~else_:(float_array_set block index value dbg) ~else_dbg:dbg
        )


  (* here, block and ptr are different only for bigstrings, because the
     extcall must apply to the whole bigstring block (variable [block]),
     whereas the loads apply to the bigstring data pointer (variable [ptr]).
     For regular strings, [block = ptr]. *)
  let string_like_load_aux ~dbg kind block ptr idx =
    begin match width with
    | Flambda_primitive.Eight ->
        load ~dbg Cmm.Byte_unsigned Cmm.Mutable (C.add_int ptr idx dbg)
    | Flambda_primitive.Sixteen ->
        unaligned_load_16 ptr idx dbg
    | Flambda_primitive.Thiry_two ->
        unaligned_load_32 ptr idx dbg
    | Flambda_primitive.Sixty_four ->
        if arch32 then
          begin match kind with
          | Flambda_primitive.String ->
              C.extcall ~alloc:false
                "caml_string_get_64" typ_int64 [block; idx]
          | Flambda_primitive.Bytes ->
              C.extcall ~alloc:false
                "caml_bytes_get_64" typ_int64 [block; idx]
          | Flambda_primitive.Bigstring ->
              C.extcall ~alloc:false
                "caml_ba_uint8_get64" typ_int64 [block; idx]
          end
        else
          unaligned_load_64 ptr idx dbg
    end

  let string_like_load ?(dbg=Debuginfo.none) kind width block index =
    match kind with
    | Flambda_primitive.String
    | Flambda_primitive.Bytes ->
        string_like_load_aux ~dbg kind block block index
    | Flambda_primitive.Bigstring ->
        let ba_data_addr = field_address block 1 dbg in
        let ba_data = load ~dbg Cmm.Word_int Cmm.Mutable ba_data_addr in
        bind "ba_data" ba_data (fun ptr ->
            string_like_load_aux ~dbg kind block ptr index)

  (* same as {string_like_load_aux} *)
  let bytes_like_set_aux ~dbg kind block ptr idx value =
    begin match width with
    | Flambda_primitive.Eight ->
        store ~dbg Cmm.Byte_unsigned Cmm.Assignment (C.add_int ptr idx dbg)
    | Flambda_primitive.Sixteen ->
        unaligned_set_16 ptr idx value dbg
    | Flambda_primitive.Thiry_two ->
        unaligned_set_32 ptr idx value dbg
    | Flambda_primitive.Sixty_four ->
        if arch32 then
          begin match kind with
          | Flambda_primitive.Bytes ->
              C.extcall ~alloc:false
                "caml_bytes_set_64" typ_int64 [block; idx; value]
          | Flambda_primitive.Bigstring ->
              C.extcall ~alloc:false
                "caml_ba_uint8_set64" typ_int64 [block; idx; value]
          end
        else
          unaligned_set_64 ptr idx value dbg
    end

  let bytes_like_set ?(dbg=Debuginfo.none) kind width block index value =
    match kind with
    | Flambda_primitive.Bytes ->
        bytes_like_set_aux ~dbg kind block block index value
    | Flambda_primitive.Bigstring ->
        let ba_data_addr = field_address block 1 dbg in
        let ba_data = load ~dbg Cmm.Word_int Cmm.Mutable ba_data_addr in
        bind "ba_data" ba_data (fun ptr ->
            bytes_like_set_aux ~dbg kind block ptr index value)

  (* wrappers for bigarrays *)

  let lambda_ba_layout = function
    | Flambda_primitive.C -> Lambda.Pbigarray_c_layout
    | Flambda_primitive.Fortran -> Lambda.Pbigarray_fortran_layout
    | Flambda_primitive.Unknown -> Lambda.Pbigarray_unknown_layout

  let lambda_ba_kind = function
    | Flambda_primitive.Unknown -> Lambda.Pbigarray_unknown
    | Flambda_primitive.Float32 -> Lambda.Pbigarray_float32
    | Flambda_primitive.Float64 -> Lambda.Pbigarray_float64
    | Flambda_primitive.Sint8   -> Lambda.Pbigarray_sint8
    | Flambda_primitive.Uint8   -> Lambda.Pbigarray_uint8
    | Flambda_primitive.Sint16  -> Lambda.Pbigarray_sint16
    | Flambda_primitive.Uint16  -> Lambda.Pbigarray_uint16
    | Flambda_primitive.Int32   -> Lambda.Pbigarray_int32
    | Flambda_primitive.Int64   -> Lambda.Pbigarray_int64
    | Flambda_primitive.Int_width_int -> Lambda.Pbigarray_caml_int
    | Flambda_primitive.Targetint_width_int -> Lambda.Pbigarray_native_int
    | Flambda_primitive.Complex32 -> Lambda.Pbigarray_complex32
    | Flambda_primitive.Complex64 -> Lambda.Pbigarray_complex64

  let bigarray_load ?(dbg=Debuginfo.none) _dims kind layout = function
    | ba :: args ->
        assert (List.length args = _dims); (* TODO: correct ? *)
        let kind = lambda_ba_kind kind in
        let layout = lambda_ba_layout layout in
        bigarray_get true kind layout ba args dbg
    | _ -> assert false

  let bigarray_store ?(dbg=Debuginfo.none) _dims kind layout = function
    | ba :: args ->
        let indexes, value = Misc.split_last args in
        let kind = lambda_ba_kind kind in
        let layout = lambda_ba_layout layout in
        bigarray_set true kind layout ba indexes value dbg
    | _ -> assert false

end



(* Name expressions *)

let symbol _env s =
  Linkage_name.to_string (Symbol.linkage_name s)

let name env = function
  | Name.Var v -> C.var (get_variable env v)
  | Name.Symbol s -> C.symbol (symbol env s)


(* Constants *)

let tag_targetint t = Targetint.(add (shift_left t 1) one)
let targetint_of_imm i = Targetint.Ocaml.to_targetint i.Immediate.value

let const _env = function
  | Simple.Const.Naked_immediate i ->
      C.targetint (targetint_of_imm i)
  | Simple.Const.Tagged_immediate i ->
      C.targetint (tag_targetint (targetint_of_imm i))
  | Simple.Const.Naked_float f ->
      C.float (Numbers.Float_by_bit_pattern.to_float f)
  | Simple.Const.Naked_int32 i -> C.int32 i
  | Simple.Const.Naked_int64 i -> C.int64 i
  | Simple.Const.Naked_nativeint t -> C.targetint t


(* Discriminants *)

let discriminant _env d =
  C.targetint (Targetint.Ocaml.to_targetint (Discriminant.to_int d))


(* 'Simple' expression *)

let simple env = function
  | Simple.Name n -> name env n
  | Simple.Const c -> const env c
  | Simple.Discriminant d -> discriminant env d


(* Arithmetic primitives *)

let primitive_boxed_int_of_standard_int = function
  | Flambda_kind.Standard_int.Naked_int32 -> Primitive.Pint32
  | Flambda_kind.Standard_int.Naked_int64 -> Primitive.Pint64
  | Flambda_kind.Standard_int.Naked_nativeint -> Primitive.Pnativeint
  | Flambda_kind.Standard_int.Tagged_immediate -> assert false

let unary_int_arith_primitive env dbg kind op arg =
  match kind with
  | Flambda_kind.Standard_int.Tagged_immediate ->
      begin match op with
      | Flambda_primitive.Neg -> C.negint arg dbg
      | Flambda_primitive.Swap_byte_endianness ->
          let untagged = C.untag_int arg dbg in
          let swapped = C.bswap16 Primitive.Pnativeint untagged dbg in
          C.tag_int swapped dbg
      end
  (* Special case for manipulating int64 on 32-bit hosts *)
  | Flambda_kind.Standard_int.Naked_int64
    when arch32 && op = Flambda_primitive.Neg ->
      C.extcall ~alloc:false "caml_int64_neg_native" typ_int64 arg
  (* General case (including byte swap for 64-bit on 32-bit archi) *)
  | _ ->
      begin match op with
      | Flambda_primitive.Neg -> C.sub_int (C.int 0) arg dbg
      | Flambda_primitive.Swap_byte_endianness ->
          let primitive_kind = primitive_boxed_int_of_standard_int kind in
          C.bbswap primitive_kind arg dbg
      end

let unary_float_arith_primitive env dbg op arg =
  match op with
  | Flambda_primitive.Abs -> C.float_abs ~dbg arg
  | Flambda_primitive.Beg -> C.float_neg ~dbg arg

let arithmetic_conversion dbg src dst arg =
  let open Flambda_kind.Standard_int_or_float in
  match src, dst with
  (* 64-bit on 32-bit host specific cases *)
  | Naked_int64, Tagged_immediate when arch32 ->
      C.extcall ~alloc:false "caml_int64_to_int" typ_int arg
  | Naked_int64, Naked_int32 when arch32 ->
      C.extcall ~alloc:false "caml_int64_to_int32" typ_int arg
  | Naked_int64, Naked_nativeint when arch32 ->
      C.extcall ~alloc:false "caml_int64_to_nativeint" typ_int arg
  | Naked_int64, Naked_float when arch32 ->
      C.extcall ~alloc:false "caml_int64_to_float_unboxed" typ_float arg
  | Tagged_immediate, Naked_int64 when arch32 ->
      C.extcall ~alloc:true "caml_int64_of_int" typ_val arg
      |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  | Naked_int32, Naked_int64 when arch32 ->
      C.extcall ~alloc:true "caml_int64_of_int32" typ_val arg
      |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  | Naked_nativeint, Naked_int64 when arch32 ->
      C.extcall ~alloc:true "caml_int64_of_nativeint" typ_val arg
      |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  | Naked_float, Naked_int64 when arch32 ->
      C.extcall ~alloc:true "caml_int64_of_float_unboxed" typ_val arg
      |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  (* general cases between integers *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Tagged_immediate ->
      C.force_tag_int arg dbg
  | Tagged_immediate, (Naked_int32 | Naked_int64 | Naked_nativeint) ->
      C.untag_int arg dbg
  (* TODO: insert shifts to zero-out higher-order bits during
           the 64 to 32 bit conversion ? *)
  | Tagged_immediate, Tagged_immediate
  | Naked_int32, (Naked_int32 | Naked_int64 | Naked_nativeint)
  | Naked_int64, (Naked_int32 | Naked_int64 | Naked_nativeint)
  | Naked_nativeint, (Naked_int32 | Naked_int64 | Naked_nativeint) ->
      arg
  (* Int-Float conversions *)
  | (Tagged_immediate | Naked_int32 | Naked_int64 | Naked_nativeint),
    Naked_float ->
      C.float_of_int ~dbg arg
  | Naked_float,
    (Tagged_immediate | Naked_int32 | Naked_int64 | Naked_nativeint) ->
      C.inf_of_float ~dbg arg

let binary_phys_comparison env dbg kind op x y =
  match kind, eq with
  (* int64 special case *)
  | Flambda_kind.Naked_number
      Flambda_kind.Naked_number_kind.Naked_int64, Flambda_primitive.Eq
    when arch32 ->
      C.extcall ~alloc:true "caml_equal" typ_int
        [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  | Flambda_kind.Naked_number
      Flambda_kind.Naked_number_kind.Naked_int64, Flambda_primitive.Neq
    when arch32 ->
      C.extcall ~alloc:true "caml_notequal" typ_int
        [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  (* General case *)
  | _, Eq -> C.eq ~dbg x y
  | _, Neq -> C.neq ~dbg x y

let binary_int_arith_primitive env dbg kind op x y =
  let open Flambda_kind.Standard_int in
  match kind, op with
  (* Int64 bits ints on 32-bit archs *)
  | Naked_int64, Flambda_primitive.Add when arch32 ->
      C.extcall ~alloc:false "caml_int64_add_native" typ_int64 [x; y]
  | Naked_int64, Flambda_primitive.Sub when arch32 ->
      C.extcall ~alloc:false "caml_int64_sub_native" typ_int64 [x; y]
  | Naked_int64, Flambda_primitive.Mul when arch32 ->
      C.extcall ~alloc:false "caml_int64_mul_native" typ_int64 [x; y]
  | Naked_int64, Flambda_primitive.Div when arch32 ->
      C.extcall ~alloc:true "caml_int64_div_native" typ_int64 [x; y]
  | Naked_int64, Flambda_primitive.Mod when arch32 ->
      C.extcall ~alloc:true "caml_int64_mod_native" typ_int64 [x; y]
  | Naked_int64, Flambda_primitive.And when arch32 ->
      C.extcall ~alloc:false "caml_int64_and_native" typ_int64 [x; y]
  | Naked_int64, Flambda_primitive.Or when arch32 ->
      C.extcall ~alloc:false "caml_int64_or_native" typ_int64 [x; y]
  | Naked_int64, Flambda_primitive.Xor when arch32 ->
      C.extcall ~alloc:false "caml_int64_xor_native" typ_int64 [x; y]
  (* Tagged integers *)
  | Tagged_immediate, Flambda_primitive.Add -> C.add_int_caml x y dbg
  | Tagged_immediate, Flambda_primitive.Sub -> S.sub_int_caml x y dbg
  | Tagged_immediate, Flambda_primitive.Mul -> C.mul_int_caml x y dbg
  | Tagged_immediate, Flambda_primitive.Div ->
      C.div_int_caml x y Lambda.Unsafe dbg
  | Tagged_immediate, Flambda_primitive.Mod ->
      C.mod_int_caml x y Lambda.Unsafe dbg
  | Tagged_immediate, Flambda_primitive.And -> C.and_int_caml x y dbg
  | Tagged_immediate, Flambda_primitive.Or  -> C.or_int_caml x y dbg
  | Tagged_immediate, Flambda_primitive.Xor -> C.xor_int_caml x y dbg
  (* Naked ints *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Flambda_primitive.Add ->
      C.add_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Flambda_primitive.Sub ->
      C.sub_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Flambda_primitive.Mul ->
      C.mul_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Flambda_primitive.Div ->
      C.div_int x y Lambda.Unsafe dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Flambda_primitive.Mod ->
      C.mod_int x y Lambda.Unsafe dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Flambda_primitive.And ->
      C.and_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Flambda_primitive.Or ->
      C.or_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Flambda_primitive.Xor ->
      C.xor_int x y dbg

let binary_int_shift_primitive env dbg kind op x y =
  let open Flambda_kind.Standard_int in
  match kind, op with
  (* Int64 special case *)
  | Naked_int64, Flambda_primitive.Lsl when arch32 ->
      todo() (* caml primitives for these have no native/unboxed version *)
  | Naked_int64, Flambda_primitive.Lsr when arch32 ->
      todo() (* caml primitives for these have no native/unboxed version *)
  | Naked_int64, Flambda_primitive.Asr when arch32 ->
      todo() (* caml primitives for these have no native/unboxed version *)
  (* Tagged integers *)
  | Tagged_immediate, Flambda_primitive.Lsl -> C.lsl_int_caml x y dbg
  | Tagged_immediate, Flambda_primitive.Lsr -> C.lsr_int_caml x y dbg
  | Tagged_immediate, Flambda_primitive.Asr -> C.asr_int_caml x y dbg
  (* Naked ints *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Flambda_primitive.Lsl ->
      C.lsl_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Flambda_primitive.Lsr ->
      C.lsr_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Flambda_primitive.Asr ->
      C.asr_int x y dbg

let binary_int_comp_primitive env dbg kind signed cmp x y =
  match kind, signed, cmp with
  | Naked_int64, Signed, Lt when arch32 ->
      C.extcall ~alloc:true "caml_lessthan" typ_int
        [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  | Naked_int64, Signed, Le when arch32 ->
      C.extcall ~alloc:true "caml_lessequal" typ_int
        [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  | Naked_int64, Signed, Gt when arch32 ->
      C.extcall ~alloc:true "caml_greaterthan" typ_int
        [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  | Naked_int64, Signed, Ge when arch32 ->
      C.extcall ~alloc:true "caml_greaterequal" typ_int
        [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  | Naked_int64, Unsigned, (Lt | Le | Gt | Ge) ->
      todo() (* There are no runtime C functions to do that afaict *)
  (* Tagged integers *)
  | Tagged_immediate, Signed, Lt -> C.tag_int (C.lt ~dbg x y) dbg
  | Tagged_immediate, Signed, Le -> C.tag_int (C.le ~dbg x y) dbg
  | Tagged_immediate, Signed, Gt -> C.tag_int (C.gt ~dbg x y) dbg
  | Tagged_immediate, Signed, Ge -> C.tag_int (C.ge ~dbg x y) dbg
  | Tagged_immediate, Unsigned, Lt -> C.tag_int (C.ult ~dbg x y) dbg
  | Tagged_immediate, Unsigned, Le -> C.tag_int (C.ule ~dbg x y) dbg
  | Tagged_immediate, Unsigned, Gt -> C.tag_int (C.ugt ~dbg x y) dbg
  | Tagged_immediate, Unsigned, Ge -> C.tag_int (C.uge ~dbg x y) dbg
(* Naked integers *)
  | (Naked_int32|Naked_int64|Naked_nativeint), Signed, Lt -> C.lt ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint), Signed, Le -> C.le ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint), Signed, Gt -> C.gt ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint), Signed, Ge -> C.ge ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint), Unsigned, Lt -> C.ult ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint), Unsigned, Le -> C.ule ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint), Unsigned, Gt -> C.ugt ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint), Unsigned, Ge -> C.uge ~dbg x y

let binary_float_arith_primitive env dbg op x y =
  match op with
  | Flambda_primitive.Add -> C.float_add ~dbg x y
  | Flambda_primitive.Sub -> C.float_sub ~dbg x y
  | Flambda_primitive.Mul -> C.float_mul ~dbg x y
  | Flambda_primitive.Div -> C.float_div ~dbg x y

let binary_float_comp_primitive env dbg x y =
  match op with
  | Eq -> C.float_eq ~dbg x y
  | Neq -> C.float_neq ~dbg x y
  | Lt -> C.float_lt ~dbg x y
  | Gt -> C.float_gt ~dbg x y
  | Le -> C.float_le ~dbg x y
  | Ge -> C.float_ge ~dbg x y

(* Primitives *)

let ba_dimension_offset layout total_dim dim =
  match layout with
  | Lambda.Pbigarray_unknown_layout -> assert false
  | Lambda.Pbigarray_fortran_layout -> 4 + dimension
  | Lambda.Pbigarray_c_layout -> 5 + total_dim - dimension

let unary_primitive env dbg f arg =
  match f with
  | Flambda_primitive.Duplicate_block _ ->
      C.extcall ~alloc:true "caml_obj_dup" typ_val [arg]
  | Flambda_primitive.Is_int ->
      C.tag_int (C._and ~dbg arg (C.int ~dbg 1)) dbg
  | Flambda_primitive.Get_tag _ ->
      C.get_tag arg dbg
  | Flambda_primitive.Discriminant_of_int ->
      simple env x
  | Flambda_primitive.Array_length block_access_kind ->
      C.block_length ~dbg block_access_kind arg
  | Flambda_primitive.Bigarray_length { dimension } ->
      (* TODO: need the bigarray layout here !! + check the dimension offset computation *)
      let dim_ofs = ba_dimension_offset (todo()) (todo()) dimension in
      C.load ~dbg Cmm.Word_int Cmm.Mutable (C.field_address arg dim_ofs)
  | Flambda_primitive.String_length _ ->
      C.string_length arg dbg
  | Flambda_primitive.Int_as_pointer ->
      C.int_as_pointer arg dbg
  | Flambda_primitive.Opaque_identity ->
      arg
  | Flambda_primitive.Int_arith (kind, op) ->
      unary_int_arith_primitive env dbg kind op arg
  | Flambda_primitive.Float_arith op ->
      unary_float_arith_primitive env dbg op arg
  | Flambda_primitive.Num_conv { src; dst; } ->
      arithmetic_conversion dbg src dst arg
  | Flambda_primitive.Boolean_not ->
      C.mk_not arg dbg
  | Flambda_primitive.Unbox_number kind ->
      C.unbox_number ~dbg kind arg
  | Flambda_primitive.Box_number kind ->
      C.box_number ~dbg kind arg
  | Flambda_primitive.Project_closure _ ->
      todo()
  | Flambda_primitive.Move_within_set_of_closures _ ->
      todo()
  | Flambda_primitive.Project_var _ ->
      todo()

let binary_primitive env dbg f x y =
  match f with
  | Flambda_primitive.Block_load (kind, _) ->
      C.block_load ~dbg kind x (C.untag_int y dbg)
  | Flambda_primitive.String_of_bigstring_load (kind, width) ->
      C.string_like_load ~dbg kind width x (C.untag_int y dbg)
  | Flambda_primitive.Phys_equal (kind, op) ->
      binary_phys_comparison env dbg kind op x y
  | Flambda_primitive.Int_arith (kind, op) ->
      binary_int_arith_primitive env dbg kind op x y
  | Flambda_primitive.Int_shift (kind, op) ->
      binary_int_shift_primitive env dbg kind op x y
  | Flambda_primitive.Int_comp (kind, signed, cmp) ->
      binary_int_comp_primitive env dbg kind signed cmp x y
  | Flambda_primitive.Float_arith op ->
      binary_float_arith_primitive env dbg op x y
  | Flambda_primitive.Float_comp cmp ->
      binary_float_comp_primitive env dbg cmp x y

let ternary_primitive env dbg f x y z =
  match f with
  | Flambda_primitive.Block_set (block_access, init) ->
      C.block_set ~dbg block_access init x (C.untag_int y dbg) z
  | Flambda_primitive.Bytes_or_bigstring_set (kind, width) ->
      C.bytes_like_set ~dbg kind width x y z

let variadic_primitive env dbg f args =
  match f with
  | Flambda_primitive.Make_block (kind, mut) ->
      C.make_block ~dbg kind mut args
  | Flambda_primitive.Bigarray_load (dimensions, kind, layoout) ->
      C.bigarray_load ~dbg dimensions kind layout args
  | Flambda_primitive.Bigarray_set (dimensions, kind, layoout) ->
      C.bigarray_store ~dbg dimensions kind layout args

let prim env dbg = function
  | Flambda_primitive.Unary (f, x) ->
      unary_primitive env dbg f (simple env x)
  | Flambda_primitive.Binary (f, x, y) ->
      binary_primitive env dbg f (simple env x) (simple env y)
  | Flambda_primitive.Ternary (f, x, y, z) ->
      ternary_primitive env dbg f (simple env x) (simple env y) (simple env z)
  | Flambda_primitive.Variadic (f, l) ->
      variadic_primitive env dbg f (List.map (simple env) l)


(* Expressions *)

let rec expr env e =
  match Expr.descr e with
  | Expr.Let e' -> let_expr env e'
  | Expr.Let_cont e' -> let_cont env e'
  | Expr.Apply e' -> apply_expr env e'
  | Expr.Apply_cont e' -> apply_cont env e'
  | Expr.Switch e' -> switch env e'
  | Expr.Invalid e' -> invalid env e'

and named env = function
  | Named.Simple s -> simple env s
  | Named.Prim (p, dbg) -> prim env dbg p
  | Named.Set_of_closures s -> set_of_closures env s

and let_expr env t =
  Let_expr.pattern_match t (fun ~bound_var:v ~body ->
      let e = Let_expr.defining_expr t in
      let env', v' = create_variable env v in
      C.letin v' (named env' e) (expr env' body)
    )

and let_cont env e =
  todo()

and apply_expr env e =
  todo()

and apply_cont env e =
  todo()

and switch env e =
  todo()

and invalid env e =
  todo()

and set_of_closures env s =
  todo()


