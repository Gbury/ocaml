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
  symb : string;
  field : int;
  kind : Cmm.memory_chunk;
}

module Self = Identifiable.Make(struct

    type nonrec t = t

    let compare u u' =
      match compare u.symb u'.symb with
      | 0 ->
          begin match compare u.field u'.field with
          | 0 -> compare u.kind u'.kind
          | res -> res
          end
      | res -> res

    let equal u u' = compare u u' = 0

    let hash = Hashtbl.hash

    let print fmt t =
      Format.fprintf fmt "%s[%d]{%s}"
        t.symb t.field (Printcmm.chunk t.kind)

    let output ch t =
      print (Format.formatter_of_out_channel ch) t

  end)

include Self

let mk kind symb field = { symb; field; kind; }

let to_cmm u e =
  let i = u.field in
  let kind = u.kind in
  let symb = Un_cps_helper.symbol u.symb in
  let address = Cmm_helpers.field_address symb i Debuginfo.none in
  Un_cps_helper.store kind Lambda.Root_initialization address e

