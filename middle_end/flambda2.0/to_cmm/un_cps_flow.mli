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


(** {2 Flow accumulator} *)

type t
(** Reverse flow accumulator: a map from variables to sets of field updates. *)

val top : t
(** The top flow *)

val empty : t
(** The empty acc *)

val is_empty : t -> bool
(** Is the given flow empty. *)

val bind : t -> Variable.t -> Un_cps_field_initialize.t -> t
(** Bind a variable to a field update. *)

val pop : t -> Variable.t -> (Un_cps_field_initialize.Set.t * t) option
(** Try and find then remove a re variable in an accumulator. *)

val find : t -> Variable.t -> Un_cps_field_initialize.Set.t
(** Try and find a variable in the acc.
    @raise Misc.fatal_error if the variable is not found. *)

val inter : t -> t -> t
(** Compute the intersection of two flows. *)

val sub : t -> t -> t
(** [sub t t'] substracts the mappings in t' from t (i.e. returns t minus
    the mappings in t'). *)

val join : t array -> t * t array
(** Try and join an array of accumulators. The single acc returned is
    the intersection of all accumulaotrs, while the difference with
    the intersection is returned in the array. *)

val flush :
  t -> (Variable.t -> Cmm.expression) -> Cmm.expression -> Cmm.expression
(** [flush t lookup e], wraps the cmm expression [e] with the updates in [t].
    The [lookup] function is used to get the cmm expression corresponding to
    an flambda2 variable (typically [Un_cps_env.get_variable]). *)

val join_and_flush :
  (Variable.t -> Cmm.expression) ->
  (Cmm.expression * t) array ->
  t * Cmm.expression array
(** Perform a join of the flows in the array, and flush the residual flows in each
    cell of the array. *)
