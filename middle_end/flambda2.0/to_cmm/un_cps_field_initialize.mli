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

(* Field initlizations *)

include Identifiable.S

val mk : Cmm.memory_chunk -> string -> int -> t
(** [mk kind symb n] create a field update that targets the [n]-th field
    of the cmm symbol [symb]. *)

val to_cmm : t -> Cmm.expression -> Cmm.expression
(** [to_cmm u e] is the cmm expression that performs initializes the field
    specified by [u] using the expression [e]. *)
