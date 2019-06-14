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

(* TODO: remove all uses of this, ^^ *)
let todo () = failwith "Not yet implemented"

(* Are we compiling on/for a 32-bit architecture ? *)
let arch32 = Arch.size_int = 32

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


  (* Constructors for operations *)

  let unary op = (fun ?(dbg=Debuginfo.none) x -> Cmm.Cop (op, [x], dbg))
  let binary op = (fun ?(dbg=Debuginfo.none) x y -> Cmm.Cop (op, [a; b], dbg))

  let _or = binary Cmm.Cor
  let _and = binary Cmm.Cand

  let add = binary Cmm.Caddi
  let sub = binary Cmm.Csubi

  let float_abs = unary Cmm.Cabsf
  let float_neg = unary Cmm.Cnegf

  let int_of_float = unary Cmm.Cintoffloat
  let float_of_int = unary Cmm.Cfloatofint


  let load ?(dbg=Debuginfo.none) kind mut addr =
    Cmm.Cop (Cmm.Cload (kind, mut), [addr], dbg)

  let extcall ?(dbg=Debuginfo.none) ?label ~alloc name typ_res args =
    Cmm.Cop (Cextcall (name, typ_res, alloc, label), args, dbg)

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
  | Flambda_kind.Standard_int.Naked_int64 when arch32 ->
      begin match op with
      | Flambda_primitive.Neg ->
          C.extcall ~alloc:false "caml_int64_neg" typ_int64 arg
      | Flambda_primitive.Swap_byte_endianness ->
          C.bbswap Primitive.Pnativeint arg dbg
      end
  | _ ->
      let primitive_kind = match kind with
        | Flambda_kind.Standard_int.Naked_int32 -> Primitive.Pint32
        | Flambda_kind.Standard_int.Naked_int64 -> Primitive.Pint64
        | Flambda_kind.Standard_int.Naked_nativeint -> Primitive.Pnativeint
      in
      begin match op with
      | Flambda_primitive.Neg -> C.sub ~dbg (C.int 0) arg
      | Flambda_primitive.Swap_byte_endianness ->
          C.bbswap primitive_kind arg dbg
      end

let unary_float_arith_primitive env dbg op arg =
  match op with
  | Flambda_primitive.Abs -> C.float_abs ~dbg arg
  | Flambda_primitive.Beg -> C.float_neg ~dbg arg

let arithmetic_conversion dbg src dst arg =
  let open Flambda_kind.Standard_int_or_float in
  match src, dst with
  | Tagged_immediate, Tagged_immediate -> arg
  | Tagged_immediate, Naked_int32
  | Tagged_immediate, Naked_int64
  | Tagged_immediate, Naked_nativeint -> C.untag_int arg dbg
  | Tagged_immediate, Naked_float -> C.float_of_int ~dbg arg
  | Naked_int32, Tagged_immediate -> tag_int arg dbg
  | Naked_int32, Naked_int32
  | Naked_int32, Naked_int64
  | Naked_int32, Naked_nativeint

(* Primitives *)

(* TODO: check semantics of array length for floats *)
let block_length = function
  | Flambda_primitive.Block_access_kind.(Block Naked_float)
  | Flambda_primitive.Block_access_kind.(Array Naked_float) ->
      C.float_array_length
  | _ -> C.addr_array_length

let ba_dimension_offset layout total_dim dim =
  match layout with
  | Lambda.Pbigarray_unknown_layout -> assert false
  | Lambda.Pbigarray_fortran_layout -> 4 + dimension
  | Lambda.Pbigarray_c_layout -> 5 + total_dim - dimension

let unary_primitive env dbg f arg =
  match f with
  | Flambda_primitive.Duplicate_block _ ->
      C.extcall ~alloc:true "caml_obj_dup" Cmm.typ_val [arg]
  | Flambda_primitive.Is_int ->
      C.tag_int (C._and ~dbg arg (C.int ~dbg 1)) dbg
  | Flambda_primitive.Get_tag _ ->
      C.get_tag arg dbg
  | Flambda_primitive.Discriminant_of_int ->
      simple env x
  | Flambda_primitive.Array_length block_access_kind ->
      C._or ~dbg (block_length block_access_kind arg dbg) (C.int 1)
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

  | _ -> todo ()

let binary_primitive env dbg f x y =
  todo()

let ternary_primitive env dbg f x y z =
  todo()

let variadic_primitive env dbg f args =
  todo()

let prim env dbg = function
  | Flambda_primitive.Unary (f, x) -> unary_primitive env dbg f x
  | Flambda_primitive.Binary (f, x, y) -> binary_primitive env dbg f x y
  | Flambda_primitive.Ternary (f, x, y, z) -> ternary_primitive env dbg f x y z
  | Flambda_primitive.Variadic (f, l) -> variadic_primitive env dbg f l


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
      Cmm.Clet (v', named env' e, expr env' body)
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


