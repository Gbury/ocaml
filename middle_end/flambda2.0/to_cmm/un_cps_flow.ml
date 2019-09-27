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

open! Flambda.Import

module I = Un_cps_field_initialize

let todo() = assert false

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
   exception). *)


(* Flow

   Upwards accumulator for reverse data-flow analysis. Maps variables
   to a set of updates/initializations that need to be performed
   using the value of the given variable. This is semantically the same
   as a map from updates/initializations to variables, but the map from
   variables was chosen becasue lookups actually are made on the variables. *)
module Flow = struct

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
    | Top -> true
    | Map { map } -> Variable.Map.is_empty map

  let mem t v =
    match t with
    | Top -> false
    | Map { map } ->
        try not (I.Set.is_empty (Variable.Map.find v map))
        with Not_found -> false

  let find t v =
    match t with
    | Top ->
        I.Set.empty
    | Map { map } ->
        try Variable.Map.find v map
        with Not_found -> I.Set.empty

  let add t v s =
    match t with
    | Top -> assert false
    | Map { map } ->
        assert (I.Set.is_empty (find t v));
        let map = Variable.Map.add v s map in
        Map { map }

  let pop t v =
    match t with
    | Top -> I.Set.empty, t
    | Map { map } ->
        begin match Variable.Map.find v map with
        | exception Not_found -> I.Set.empty, t
        | res ->
            let map = Variable.Map.remove v map in
            res, Map { map }
        end

  let filter t s =
    match t with
    | Top -> t
    | Map { map } ->
        let map = Variable.Map.filter (fun v _ -> Variable.Set.mem v s) map in
        Map { map }

  (* Flow intersection *)
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

  (* Compute the join point of multiple control flows *)
  let join arr =
    let res = inters arr in
    let aux t = sub t res in
    res, Array.map aux arr

  (* Flush all the updates in a flow *)
  let flush t e =
    match t with
    | Top -> e
    | Map { map } ->
        Variable.Map.fold (fun v s acc ->
            I.Set.fold (fun i acc ->
                let named = I.to_flambda i (Simple.var v) in
                let tmp = Variable.create "*module_init*" in
                let name_occ = Name_occurrence_kind.normal in
                let var = Var_in_binding_pos.create tmp name_occ in
                Expr.create_let var named acc
              ) s acc
          ) map e

end

(* Flow of a bound continuation

   Continuations that are bound/defined have a two part flow:
   - the flow of their arguments
   - the flow of variables in the environment

*)
module Flow_cont = struct

  type t = {
    env : Flow.t;
    vars : I.Set.t list;
  }

  let mk env vars = { env; vars; }

  let is_empty { env; vars; } =
    Flow.is_empty env &&
    List.for_all I.Set.is_empty vars

  let inter t t' =
    let env = Flow.inter t.env t'.env in
    let vars = List.map2 I.Set.inter t.vars t'.vars in
    mk env vars

  let inters arr =
    assert (Array.length arr > 0);
    let tmp = ref arr.(0) in
    for i = 1 to Array.length arr - 1 do
      tmp := inter !tmp arr.(i)
    done;
    !tmp

  let sub t t' =
    let env = Flow.sub t.env t'.env in
    let vars = List.map2 I.Set.diff t.vars t'.vars in
    mk env vars

  let join arr =
    let res = inters arr in
    let aux t = sub t res in
    res, Array.map aux arr

  let merge t t' =
    let res = inter t t' in
    res, sub t res, sub t' res

  let to_flow { env; vars; } l =
    List.fold_left2 Flow.add env l vars

  let flush t k =
    if is_empty t then
      k, Fun.id
    else begin
      let k' = Continuation.create () in
      let flow = to_flow 
    end

end

(* Accumulator for expression rewriting.

   Basically a map from continuations to the list of initializations for each
   argument of the continuation. *)
module Acc = struct

  type t = {
    map : Flow_cont.t Continuation.Map.t;
    lets : (Bindable_let_bound.t * Named.t) list;
    conts : Continuation_handler.t Continuation.Map.t;
  }

  (* Add the resulting flow of a let-bound continuation to the accumulator *)
  let register t k vars flow =
    let l, flow = List.fold_left (fun (acc, flow) v ->
        let s, flow = Flow.pop flow v in
        (s :: acc, flow)
      ) ([], flow) vars in
    let flow_cont = Flow_cont.mk flow (List.rev l) in
    { t with map = Continuation.Map.add k flow_cont t.map; }

  (* The Not_found exception is not catched here, but later, in order to
     provide better context for the error message. *)
  let find t k = Continuation.Map.find k t.map

  (* When applying a continuation to some arguments, compute the resulting flow*)
  let apply t k args =
    let Flow_cont.{ env; vars; } = find t k in
    List.fold_left2 (fun flow e s ->
        match Simple.descr e with
        | Name Var v -> Flow.add flow v s
        | _ ->
            if I.Set.is_empty s then flow
            else
              (* This should definitely not happen *)
              Misc.fatal_errorf
                "Non-empty flow assigned to non-variable simple: '%a'"
                Simple.print e
      ) env args vars

  (* Add a let-binding *)
  let bind t vars n =
    { t with lets = (vars, n) :: t.lets; }

  (* Flush the delayed let-bindings *)
  let flush_lets t =
    let l = t.lets in
    let t' = { t with lets = []; } in
    let wrap e flow =
      List.fold_left (fun acc (v, n) ->
          let bvars = Bindable_let_bound.all_bound_vars' v in
          let flow' = Flow.filter flow bvars in
          let acc = Flow.flush flow' acc in
          Expr.create_pattern_let v n acc
        ) e l in
    t', wrap

end

(* Expression rewriting

   The idea is to perform the flow analysis, and using that to determine the
   earliest point at which module field initializations can be safely done,
   and insert explicit uses of the [Block_set] primitive in the flambda term
   at those points. *)
module Rewrite = struct

  let rec expr acc e =
    match Expr.descr e with
    | Let e' -> let_expr acc e'
    | Let_cont e' -> let_cont acc e'
    | Apply e' -> apply_expr acc e'
    | Apply_cont e' -> apply_cont acc e e'
    | Switch e' -> switch acc e'
    | Invalid e' -> invalid acc e e'

  and let_expr acc e =
    let n = Let.defining_expr e in
    Let.pattern_match e ~f:(fun ~bound_vars ~body ->
        let acc = Acc.bind acc bound_vars n in
        expr acc body
      )

  and let_cont _acc _e =
    todo()

  and apply_expr acc e =
    let k = Apply.continuation e in
    let k_exn, k_exn_args =
      let tmp = Apply.exn_continuation e in
      Exn_continuation.exn_handler tmp, Exn_continuation.extra_args tmp
    in
    let k_flow = Acc.find acc k in
    let k_exn_flow = Acc.find acc k_exn in
    todo()


  and apply_cont acc t e =
    let k = Apply_cont.continuation e in
    let args = Apply_cont.args e in
    let flow = Acc.apply acc k args in
    flow, t

  and switch _acc _e =
    todo()

  and invalid _ t _ = Flow.top, t

end
