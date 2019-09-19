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

module Structure : sig

  type t
  (** The result of compiling a top-level static structure, with data
      items for the static part, and field updates for the values
      computed dynamically. *)

  val empty : t
  (** The empty result. *)

  val add_data : Cmm.data_item -> t -> t
  (** Add cmm data to the front of the data list. *)

  val update_data : (Cmm.data_item list -> Cmm.data_item list) -> t -> t
  (** Update the data item list of the result. *)

  val add_update : Variable.t -> Un_cps_field_initialize.t -> t -> t
  (** Bind a variable to a field initialization. *)

  val destruct : t -> Cmm.data_item list * Un_cps_flow.t
  (** Destruct a structure result into data and flow. *)

end


module Definition : sig

  type t
  (** The result of compiling any number of top-level definitions
      (with data for the static part, and an init expression). *)

  val empty : t
  (** The empty result. *)

  val add_data : Cmm.data_item list -> t -> t
  (** Add a list of data items to the given result. *)

  val prepend : Cmm.expression -> t -> t
  (** Pre-pend an expression to the init part of a result. *)

  val combine : t -> t -> t
  (** Combine two definition results. *)

  val to_cmm : t -> Cmm.phrase list * Cmm.phrase
  (** Compute the phrases for data items, and the phrase for the initialization
      code of the given result. *)

end
