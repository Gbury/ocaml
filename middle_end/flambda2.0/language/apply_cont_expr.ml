(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module K = Flambda_kind

type t = {
  k : Continuation.t;
  args : Simple.t list;
  trap_action : Trap_action.Option.t;
}

let print ppf { k; args; trap_action; } =
  match args with
  | [] ->
    Format.fprintf ppf "@[<2>(%a%sgoto%s@ %a)@]"
      Trap_action.Option.print trap_action
      (Misc.Color.bold_cyan ())
      (Misc.Color.reset ())
      Continuation.print k
  | _ ->
    Format.fprintf ppf "@[<2>(%a%sapply_cont%s@ %a@ %a)@]"
      Trap_action.Option.print trap_action
      (Misc.Color.bold_cyan ())
      (Misc.Color.reset ())
      Continuation.print k
      Simple.List.print args

let print_with_cache ~cache:_ ppf t = print ppf t

let invariant env ({ k; args; trap_action; } as t) =
  let module E = Invariant_env in
  let unbound_continuation cont reason =
    Misc.fatal_errorf "Unbound continuation %a in %s: %a"
      Continuation.print cont
      reason
      print t
  in
  let args_arity = List.map (fun arg -> E.kind_of_simple env arg) args in
  let arity, kind (*, cont_stack *) =
    match E.find_continuation_opt env k with
    | Some result -> result
    | None -> unbound_continuation k "[Apply_cont] term"
  in
(*
  let stack = E.current_continuation_stack env in
  E.Continuation_stack.unify cont stack cont_stack;
*)
  if not (Flambda_arity.equal args_arity arity) then begin
    Misc.fatal_errorf "Continuation %a called with wrong arity in \
        this [Apply_cont] term: expected %a but found %a:@ %a"
      Continuation.print k
      Flambda_arity.print arity
      Flambda_arity.print args_arity
      print t
  end;
  begin match kind with
  | Normal -> ()
  | Exn_handler ->
    Misc.fatal_errorf "Continuation %a is an exception handler \
        but is used in this [Apply_cont] term as a normal continuation:@ \
        %a"
      Continuation.print k
      print t
  end;
  let check_exn_handler exn_handler =
    match E.find_continuation_opt env exn_handler with
    | None ->
      unbound_continuation exn_handler "[Apply] trap handler"
    | Some (arity, kind (*, cont_stack *)) ->
      begin match kind with
      | Exn_handler -> ()
      | Normal ->
        Misc.fatal_errorf "Continuation %a is a normal continuation  \
            but is used in the trap action of this [Apply] term as an \
            exception handler:@ %a"
          Continuation.print exn_handler
          print t
      end;
      assert (not (Continuation.equal k exn_handler));
      let expected_arity = [K.value ()] in
      if not (Flambda_arity.equal arity expected_arity) then begin
        Misc.fatal_errorf "Exception handler continuation %a has \
            the wrong arity for the trap handler action of this \
            [Apply] term: expected %a but found %a:@ %a"
          Continuation.print k
          Flambda_arity.print expected_arity
          Flambda_arity.print arity
          print t
      end;
      ()
(*
      cont_stack
*)
  in
(*
  let current_stack = E.current_continuation_stack env in
*)
  (* CR mshinwell for pchambart: We need to fix this.  I've removed the
     trap IDs since we don't need them for compilation, and they would be
     another kind of name that needs freshening (which is weird since they
     don't have any binding site). *)
(*
  let stack, cont_stack =
*)
    match trap_action with
    | None -> () (*current_stack, cont_stack *)
    | Some (Push { exn_handler }) ->
      check_exn_handler exn_handler
(*
      let cont_stack = check_exn_handler exn_handler in
      E.Continuation_stack.push id exn_handler current_stack, cont_stack
*)
    | Some (Pop { exn_handler; take_backtrace = _; }) ->
      check_exn_handler exn_handler

(*
      let cont_stack = check_exn_handler exn_handler in
      current_stack, E.Continuation_stack.push id exn_handler cont_stack
*)
(*
  in
  E.Continuation_stack.unify cont stack cont_stack
  current_stack
  *)

let create ?trap_action k ~args = { k; args; trap_action; }

let goto k =
  { k;
    args = [];
    trap_action = None;
  }

let continuation t = t.k
let args t = t.args
let trap_action t = t.trap_action

let free_names { k; args; trap_action; } =
  let trap_action_free_names =
    match trap_action with
    | None -> Name_occurrences.empty
    | Some trap_action -> Trap_action.free_names trap_action
  in
  Name_occurrences.union_list [
    Name_occurrences.singleton_in_terms (Continuation k);
    Simple.List.free_names args;
    trap_action_free_names;
  ]

let continuation_counts { k; args = _; trap_action; } =
  let trap_action_continuation_counts =
    match trap_action with
    | None -> Continuation_counts.empty
    | Some trap_action -> Trap_action.continuation_counts trap_action
  in
  Continuation_counts.union_list [
    Continuation_counts.create_singleton k;
    trap_action_continuation_counts;
  ]

let apply_name_permutation ({ k; args; trap_action; } as t) perm =
  let k' = Name_permutation.apply_continuation perm k in
  let args' = Simple.List.apply_name_permutation args perm in
  let trap_action' =
    match trap_action with
    | None -> None
    | Some trap_action' ->
      let new_trap_action' =
        Trap_action.apply_name_permutation trap_action' perm
      in
      if new_trap_action' == trap_action' then trap_action
      else Some new_trap_action'
  in
  if k == k' && args == args' && trap_action == trap_action' then t
  else { k = k'; args = args'; trap_action = trap_action'; }
