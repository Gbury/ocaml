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

(* Result for translating a program *)

module C = Un_cps_helper

(* The result fo compiling a static structure *)

module Structure = struct

  type t = {
    data : Cmm.data_item list;
    updates : Un_cps_flow.t;
  }

  let empty = {
    data = [];
    updates = Un_cps_flow.empty;
  }

  let add_data d t =
    { t with data = d :: t.data; }

  let update_data f t =
    { t with data = f t.data; }

  let add_update v u t =
    { t with updates = Un_cps_flow.bind t.updates v u; }

  let destruct { data; updates; } =
    data, updates

end

(* The result of computing a definition *)

module Definition = struct

  type t = {
    init : Cmm.expression;
    data : Cmm.data_item list list;
  }

  let empty = {
    init = C.void;
    data = [];
  }

  let add_if_not_empty x l =
    match x with
    | [] -> l
    | _ :: _ -> x :: l

  let add_data d t =
    { t with data = add_if_not_empty d t.data; }

  let prepend e t =
    { t with init = C.sequence e t.init; }

  let combine t t' = {
    init = C.sequence t.init t'.init;
    data = t.data @ t'.data;
  }

  let to_cmm r =
    let entry =
      let dbg = Debuginfo.none in
      let fun_name = Compilenv.make_symbol (Some "entry") in
      let fun_codegen =
        if Config.flambda then
          [ Cmm.Reduce_code_size;
            Cmm.No_CSE ]
        else
          [ Cmm.Reduce_code_size ]
      in
      let init = C.sequence r.init (C.unit ~dbg) in
      C.cfunction (C.fundecl fun_name [] init fun_codegen dbg)
    in
    let data = List.map C.cdata r.data in
    data, entry

end
