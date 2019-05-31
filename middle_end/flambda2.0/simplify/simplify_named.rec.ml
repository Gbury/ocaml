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

open! Flambda.Import

module E = Simplify_env_and_result.Env
module T = Flambda_type
module TE = T.Typing_env

let simplify_name_for_rhs_of_let env r name =
  let typing_env = E.typing_env env in
  let name = TE.get_canonical_name typing_env name in
  (* CR mshinwell: Avoid double lookup here *)
  let kind = T.kind (TE.find typing_env name) in
  let simple = Simple.name name in
  (* We don't resolve [name] right back to a type and return that, for
     such a procedure would cause loss of alias information. *)
  simple, T.alias_type_of kind simple, r

let simplify_simple_for_rhs_of_let env r (simple : Simple.t) =
  match simple with
  | Const c -> simple, T.type_for_const c, r
  | Discriminant t -> simple, T.this_discriminant t, r
  | Name name -> simplify_name_for_rhs_of_let env r name

let simplify_function env r closure_id function_decl ~type_of_my_closure =
  let params_and_body, r =
    Function_params_and_body.pattern_match
      (Function_declaration.params_and_body function_decl)
      ~f:(fun ~return_continuation exn_continuation params ~body
              ~my_closure ->
        let env = E.enter_closure env in
        let result_arity = Function_declaration.result_arity function_decl in
        let env = E.add_continuation env return_continuation result_arity in
        let env = E.add_exn_continuation env exn_continuation in
        let env = E.add_parameters_with_unknown_types env params in
        let type_of_my_closure = type_of_my_closure closure_id in
        let env = E.add_variable env my_closure type_of_my_closure in
        let env = E.increment_continuation_scope_level env in
(*
Format.eprintf "Closure ID %a env:@ %a@ function body:@ %a\n%!"
  Closure_id.print closure_id
  E.print env
  Expr.print body;
*)
        let body, r =
          try
            Simplify_toplevel.simplify_toplevel env r body
              ~return_continuation
              exn_continuation
          with Misc.Fatal_error -> begin
            Format.eprintf "\n%sContext is:%s simplifying function \
                with closure ID %a,@ params %a,@ return continuation %a,@ \
                exn continuation %a,@ my_closure %a,@ body:@ %a@ \
                in environment:@ %a@ with (outer) result structure:@ %a\n"
              (Misc.Color.bold_red ())
              (Misc.Color.reset ())
              Closure_id.print closure_id
              Kinded_parameter.List.print params
              Continuation.print return_continuation
              Exn_continuation.print exn_continuation
              Variable.print my_closure
              Expr.print body
              E.print env
              R.print r;
            raise Misc.Fatal_error
          end
        in
        let function_decl =
          Function_params_and_body.create ~return_continuation
            exn_continuation params ~body ~my_closure
        in
        function_decl, r)
  in
  let function_decl =
    Function_declaration.update_params_and_body function_decl params_and_body
  in
  let function_decl_type =
    if Inlining_decision.can_inline env function_decl then
      T.create_inlinable_function_declaration function_decl
    else
      T.create_non_inlinable_function_declaration ()
  in
  function_decl, function_decl_type, r

let simplify_set_of_closures0 env r set_of_closures ~result_var =
  let closure_elements, closure_element_types, r =
    Var_within_closure.Map.fold
      (fun var_within_closure simple
           (closure_elements, closure_element_types, r) ->
        let simple, ty = Simplify_simple.simplify_simple env simple in
        let closure_elements =
          Var_within_closure.Map.add var_within_closure simple
            closure_elements
        in
        let ty_value = T.force_to_kind_value ty in
        let closure_element_types =
          Var_within_closure.Map.add var_within_closure ty_value
            closure_element_types
        in
        closure_elements, closure_element_types, r)
      (Set_of_closures.closure_elements set_of_closures)
      (Var_within_closure.Map.empty, Var_within_closure.Map.empty, r)
  in
  let internal_closure_element_types =
    Var_within_closure.Map.map (fun ty_value ->
        T.erase_aliases_ty_value ty_value ~allowed:Variable.Set.empty)
      closure_element_types
  in
  let type_of_my_closure closure_id =
    (* CR mshinwell: Think more about the following. *)
    T.closure closure_id
      (T.create_non_inlinable_function_declaration ())
      internal_closure_element_types
      ~set_of_closures:(T.unknown_as_ty_fabricated ())
  in
  let function_decls = Set_of_closures.function_decls set_of_closures in
  let funs = Function_declarations.funs function_decls in
  let funs, fun_types, r =
    Closure_id.Map.fold (fun closure_id function_decl (funs, fun_types, r) ->
        let function_decl, ty, r =
          simplify_function env r closure_id function_decl ~type_of_my_closure
        in
        let funs = Closure_id.Map.add closure_id function_decl funs in
        let fun_types = Closure_id.Map.add closure_id ty fun_types in
        funs, fun_types, r)
      funs
      (Closure_id.Map.empty, Closure_id.Map.empty, r)
  in
  let function_decls = Function_declarations.create funs in
  let set_of_closures =
    Set_of_closures.create ~function_decls ~closure_elements
  in
  let closure_types =
    Closure_id.Map.mapi (fun closure_id function_decl_type ->
        T.closure closure_id function_decl_type closure_element_types
          ~set_of_closures:set_of_closures_ty_fabricated)
      fun_types
  in
  (* The resulting set-of-closures and closures types are recursive. To avoid
     having to rewrite the type afterwards in the case of lifting the set of
     closures, we determine now if lifting will happen, and if so tie the
     recursive loop via a symbol instead of a variable. *)
  let will_lift =
    Set_of_closures.environment_doesn't_mention_variables set_of_closures
  in
  if not will_lift then
    let set_of_closures_ty_fabricated =
      T.alias_type_of_as_ty_fabricated (Simple.var result_var)
    in
    let set_of_closures_type = T.set_of_closures ~closures:closure_types in
    set_of_closures, set_of_closures_type, env, r
  else
    let name = Variable.unique_name result_var in
    let set_of_closures_symbol =
      Symbol.create (Compilation_unit.get_current_exn ()) name
    in
    let closure_symbols_and_types =
      Closure_id.Map.map (fun typ ->
          let closure_symbol =
            Symbol.create (Compilation_unit.get_current_exn ())
              (Closure_id.unique_name closure_id)
          in
          closure_symbol, typ)
        closure_types
    in
    let closure_symbols =
      Closure_id.Map.map (fun (sym, typ) -> sym) closure_symbols_and_types
    in
    let static_part : Flambda_static.Static_part.t =
      Set_of_closures set_of_closures
    in
    let bound_symbols : Flambda_static.Bound_symbols.t =
      Set_of_closures {
        set_of_closures_symbol;
        closure_symbols;
      }
    in
    let set_of_closures_ty_fabricated =
      T.alias_type_of_as_ty_fabricated (Simple.var result_var)
    in
    let set_of_closures_type = T.set_of_closures ~closures:closure_types in
    let static_structure = [bound_symbols, static_part] in
    let r =
      R.new_lifted_constant r ~name set_of_closures_type static_structure
    in
    let env = E.add_symbol env symbol set_of_closures_type
    let env =
      Closure_id.Map.fold (fun _closure_id (symbol, typ) env ->
          E.add_symbol env symbol typ)
        closure_symbols_and_types
        env
    in
    let set_of_closures_symbol = Simple.symbol set_of_closures_symbol in
    let term = Named.create_simple set_of_closures_symbol in
    let ty = T.alias_type_of (T.kind ty) set_of_closures_symbol in
    let env = E.add_equation_on_variable env bound_to ty in
    term, ty, env, r

let simplify_set_of_closures env r set_of_closures ~result_var =
  let set_of_closures, ty, r =
    simplify_set_of_closures0 env r set_of_closures ~result_var
  in
  Named.create_set_of_closures set_of_closures, ty, r

let simplify_set_of_closures_and_drop_type env r set_of_closures =
  let result_var = Variable.create "set_of_closures" in
  let set_of_closures, _ty, r =
    simplify_set_of_closures0 env r set_of_closures ~result_var
  in
  set_of_closures, r

let simplify_named0 env r (named : Named.t) ~result_var =
(*Format.eprintf "Simplifying binding of %a\n%!" Variable.print result_var;*)
  match named with
  | Simple simple ->
(*let orig_simple = simple in*)
    let simple, ty, r = simplify_simple_for_rhs_of_let env r simple in
(*Format.eprintf "Simplified %a --> %a, type %a\n%!"
  Simple.print orig_simple
  Simple.print simple
  T.print ty;*)
    let env = E.add_variable env result_var ty in
    Reachable.reachable (Named.create_simple simple), env, ty, r
  | Prim (prim, dbg) ->
    let term, env_extension, r =
      Simplify_primitive.simplify_primitive env r prim dbg ~result_var
    in
    let env =
      let kind = Flambda_primitive.result_kind' prim in
      let env = E.add_variable env result_var (T.unknown kind) in
      E.extend_typing_environment env env_extension
    in
    let effects_and_coeffects_ok =
      Flambda_primitive.With_fixed_value.eligible prim
    in
    Reification.try_to_reify env r term
      ~bound_to:result_var ~cannot_lift:(not effects_and_coeffects_ok)
  | Set_of_closures set_of_closures ->
    let term, ty, r =
      simplify_set_of_closures env r set_of_closures ~result_var
    in
    let env = E.add_variable env result_var ty in
    Reification.try_to_reify env r (Reachable.reachable term)
      ~bound_to:result_var ~cannot_lift:false

let simplify_named env r named ~result_var =
  let named, env, ty, r = simplify_named0 env r named ~result_var in
  let named : Reachable.t =
    match named with
    | Invalid _ -> named
    | Reachable _ ->
      if T.is_bottom (E.typing_env env) ty then Reachable.invalid ()
      else named
  in
  env, r, ty, named
