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

module I = Un_cps_field_initialize

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

(* Flow accumulator. *)

type t =
  | Top
  | Map of {
      map : I.Set.t Variable.Map.t;
      (* Invariant: sets mapped to variables should always be non-empty. *)
    }

let top = Top

let empty =
  Map { map = Variable.Map.empty; }

let is_empty = function
  | Top -> false
  | Map { map } -> Variable.Map.is_empty map

let find t v =
  match t with
  | Top -> assert false (* TODO: what to return here ? *)
  | Map { map } ->
      try Variable.Map.find v map
      with Not_found -> I.Set.empty

let bind t v u =
  match t with
  | Top -> assert false
  | Map { map } ->
      let s = find t v in
      let map = Variable.Map.add v (I.Set.add u s) map in
      Map { map }

let pop t v =
  match t with
  | Top -> None
  | Map { map } ->
      begin match Variable.Map.find v map with
      | exception Not_found -> None
      | res ->
          let map = Variable.Map.remove v map in
          Some (res, Map {map })
      end

(* Joining accumulators. *)
let inter t t' =
  match t, t' with
  | Top, (_ as res)
  | (_ as res), Top -> res
  | Map t, Map t' ->
      let map = Variable.Map.merge (fun _ v v' ->
          match v, v' with
          | Some s, Some s' ->
              let s'' = I.Set.inter s s' in
              if I.Set.is_empty s'' then None else Some s''
          | _ -> None
        ) t.map t'.map in
      Map { map }

let inters arr =
  assert (Array.length arr > 0);
  let tmp = ref arr.(0) in
  for i = 1 to Array.length arr - 1 do
    tmp := inter !tmp arr.(i)
  done;
  !tmp

(* substract the mappings in t' from t
   (i.e. returns t minus the mappings in t').*)
let sub t t' =
  match t, t' with
  | _, Top -> empty
  | Top, Map _ -> assert false (* Can't really assign a semantics here *)
  | Map t, Map t' ->
      let map = Variable.Map.merge (fun _ o o' ->
          match o, o' with
          | None, None -> None
          | Some _, None -> o
          | Some s, Some s' ->
              let s'' = I.Set.diff s s' in
              if I.Set.is_empty s'' then None else Some s''
          | None, Some _ ->
              (* this shouldn't really happen *)
              assert false
        ) t.map t'.map in
      Map { map }

let join arr =
  let res = inters arr in
  let aux t = sub t res in
  res, Array.map aux arr

(* this takes a lookup function to avoid a dependency on un_cps_env *)
let flush t lookup e =
  match t with
  | Top -> e
  | Map t ->
      Variable.Map.fold (fun v s acc ->
          let e = lookup v in
          I.Set.fold (fun u acc ->
              Un_cps_helper.sequence (I.to_cmm u e) acc
            ) s acc
        ) t.map e

let join_and_flush lookup arr =
  let res_flow = Array.fold_left
      (fun acc (_, flow) -> inter flow acc) top arr in
  let res_arr = Array.map (fun (e, flow) ->
      let flow = sub flow res_flow in
      flush flow lookup e
    ) arr in
  res_flow, res_arr


