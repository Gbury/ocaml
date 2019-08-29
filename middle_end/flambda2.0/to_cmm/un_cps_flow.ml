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

(* (Reverse) Data flow analysis.

   The flambda to cmm translation of large modules can be problematic
   if done naively: if a lot (>10) of fields have to be updated (which
   happens regularly), the generated flambda code will pass through
   a continuation with as many arguments. This is a problem because
   it forces to keep the first computed value alive until the last
   computed value is done, and then only performs the set-block of
   the relevant fields. In order to avoid that, the idea is to peform
   a reverse data flow analysis from the continuation calls at the
   end of a computation, all the was up so that the module field update
   can be inserted at the earliest point possible, either right after
   a variable let-binding, or after a control flow split (switch or
   exception).
*)

type field_update = {
  symb : Cmm.expression;
  field : int;
}

type t = {
  map : field_update Variable.Map.t;
}

(* Replace the update bound to v' by the same but bound to v instead *)
let bind t v v' =
  match Variable.Map.find v' t.map with
  | exception Not_found ->
      Misc.fatal_errorf
        "Expected to find %a in the update map but didn't"
        Variable.print v'
  | u ->
      { (* t with *) map = Variable.Map.add v u t.map; }

