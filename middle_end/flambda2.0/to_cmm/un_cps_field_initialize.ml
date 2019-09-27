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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Field updates. *)

type t = {
  symb : Symbol.t;
  field : int;
  kind : Flambda_primitive.Block_access_kind.t0;
}

module Self = Identifiable.Make(struct

    type nonrec t = t

    let compare u u' =
      match Symbol.compare u.symb u'.symb with
      | 0 -> compare u.field u'.field
      | res -> res

    let equal u u' = compare u u' = 0

    let hash = Hashtbl.hash

    let print fmt t =
      Format.fprintf fmt "%a[%d]"
        Symbol.print t.symb t.field

    let output ch t =
      print (Format.formatter_of_out_channel ch) t

  end)

include Self

let mk kind symb field = { symb; field; kind; }

let to_flambda u s =
  let block = Simple.symbol u.symb in
  let field_imm = Immediate.int (Targetint.OCaml.of_int u.field) in
  let field = Simple.const (Simple.Const.Tagged_immediate field_imm) in
  let block_access_kind = Flambda_primitive.Block_access_kind.(Block u.kind) in
  let init = Flambda_primitive.Initialization in
  let ternary_prim = Flambda_primitive.Block_set (block_access_kind, init) in
  let prim = Flambda_primitive.Ternary (ternary_prim, block, field, s) in
  Flambda.Named.create_prim prim Debuginfo.none

(*
let to_cmm u e =
  let i = u.field in
  let kind = u.kind in
  let symb = Un_cps_helper.symbol u.symb in
  let address = Cmm_helpers.field_address symb i Debuginfo.none in
  Un_cps_helper.store kind Lambda.Root_initialization address e
*)


