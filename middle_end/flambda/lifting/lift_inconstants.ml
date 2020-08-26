(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

let matches_extension_constructor static_const =
  match (static_const : Static_const.t) with
  | Block (tag, mut, _fields) ->
    begin match mut with
    | Immutable_unique ->
      (* Only extension constructors, with an object tag, are supposed to be
         Immutable_unique *)
      assert (Tag.equal Tag.object_tag (Tag.Scannable.to_tag tag));
      true
    | Mutable | Immutable -> false
    end
  | (Code _ | Set_of_closures _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
    | Boxed_nativeint _ | Immutable_float_block _ | Immutable_float_array _
    | Mutable_string _ | Immutable_string _) -> false

let allowed_for_toplevel_lifting static_const =
  (* Always try to lift extension constructors, as they're not likely
     to create many extra equations and can often prevent sets of closures
     from being closed if not lifted. *)
  if Flambda_features.lift_toplevel_inconstants () then true
  else matches_extension_constructor static_const

type reify_primitive_at_toplevel_result =
  | Lift of {
    symbol : Symbol.t;
    static_const : Flambda.Static_const.t;
  }
  | Shared of Symbol.t
  | Cannot_reify

let reify_primitive_at_toplevel dacc bound_var ty =
  let typing_env = DA.typing_env dacc in
  (* CR mshinwell: We're reifying twice here (the other occurrence being from
     [Simplify_named].  We should probably combine this code with the code in
     [Reification] somehow to avoid this.  It should also mean we don't need
     the [is_fully_static] check below. *)
  match
    T.reify ~allowed_if_free_vars_defined_in:typing_env
      ~allow_unique:true
      typing_env ~min_name_mode:NM.normal ty
  with
  | Lift to_lift ->
    (* There's no point in lifting constant values, as these should
       already have been lifted. *)
    let static_const = Reification.create_static_const to_lift in
    if Static_const.is_fully_static static_const
      || not (allowed_for_toplevel_lifting static_const)
    then
      Cannot_reify
    else begin
      match DA.find_shareable_constant dacc static_const with
      | Some symbol -> Shared symbol
      | None ->
        let symbol =
          Symbol.create (Compilation_unit.get_current_exn ())
            (Linkage_name.create
               (Variable.unique_name (Var_in_binding_pos.var bound_var)))
        in
        Lift { symbol; static_const; }
    end
  | Lift_set_of_closures _ | Simple _ | Cannot_reify | Invalid ->
    Cannot_reify
