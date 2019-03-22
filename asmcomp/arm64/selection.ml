(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction selection for the ARM processor *)

open Arch
open Cmm
open Mach

let is_offset chunk n =
   (n >= -256 && n <= 255)               (* 9 bits signed unscaled *)
|| (n >= 0 &&
    match chunk with     (* 12 bits unsigned, scaled by chunk size *)
    | Byte_unsigned | Byte_signed ->
        n < 0x1000
    | Sixteen_unsigned | Sixteen_signed ->
        n land 1 = 0 && n lsr 1 < 0x1000
    | Thirtytwo_unsigned | Thirtytwo_signed | Single ->
        n land 3 = 0 && n lsr 2 < 0x1000
    | Word _ | Double | Double_u ->
        n land 7 = 0 && n lsr 3 < 0x1000)

(* An automaton to recognize ( 0+1+0* | 1+0+1* )

               0          1          0
              / \        / \        / \
              \ /        \ /        \ /
        -0--> [1] --1--> [2] --0--> [3]
       /
     [0]
       \
        -1--> [4] --0--> [5] --1--> [6]
              / \        / \        / \
              \ /        \ /        \ /
               1          0          1

The accepting states are 2, 3, 5 and 6. *)

let auto_table = [|   (* accepting?, next on 0, next on 1 *)
  (* state 0 *) (false, 1, 4);
  (* state 1 *) (false, 1, 2);
  (* state 2 *) (true,  3, 2);
  (* state 3 *) (true,  3, 7);
  (* state 4 *) (false, 5, 4);
  (* state 5 *) (true,  5, 6);
  (* state 6 *) (true,  7, 6);
  (* state 7 *) (false, 7, 7)   (* error state *)
|]

let rec run_automata nbits state input =
  let (acc, next0, next1) = auto_table.(state) in
  if nbits <= 0
  then acc
  else run_automata (nbits - 1)
                    (if input land 1 = 0 then next0 else next1)
                    (input asr 1)

(* We are very conservative wrt what ARM64 supports: we don't support
   repetitions of a 000111000 or 1110000111 pattern, just a single
   pattern of this kind. *)

let is_logical_immediate n =
  n <> 0 && n <> -1 && run_automata 64 0 n

(* If you update [inline_ops], you may need to update [is_simple_expr] and/or
   [effects_of], below. *)
let inline_ops =
  [ "sqrt"; "caml_bswap16_direct"; "caml_int32_direct_bswap";
    "caml_int64_direct_bswap"; "caml_nativeint_direct_bswap" ]

let use_direct_addressing _symb =
  not !Clflags.dlcode

(* Instruction selection *)

class selector = object(self)

inherit Selectgen.selector_generic as super

method is_immediate n =
  let mn = -n in
  n land 0xFFF = n || n land 0xFFF_000 = n
  || mn land 0xFFF = mn || mn land 0xFFF_000 = mn

method! is_simple_expr = function
  (* inlined floating-point ops are simple if their arguments are *)
  | Cop(Cextcall (fn, _, _, _), args, _) when List.mem fn inline_ops ->
      List.for_all self#is_simple_expr args
  | e -> super#is_simple_expr e

method! effects_of e =
  match e with
  | Cop(Cextcall (fn, _, _, _), args, _) when List.mem fn inline_ops ->
      Selectgen.Effect_and_coeffect.join_list_map args self#effects_of
  | e -> super#effects_of e

method select_addressing chunk = function
  | Cop(Cadd (_, (Must_scan | Cannot_be_live_at_gc)),
        [Cconst_symbol (s, _); Cconst_int n], _)
    when use_direct_addressing s ->
      (Ibased(s, n), Ctuple [])
  | Cop(Cadd (_, (Must_scan | Cannot_be_live_at_gc)), [arg; Cconst_int n], _)
    when is_offset chunk n ->
      (Iindexed n, arg)
  | Cop((Cadd (sz, (Must_scan | Cannot_be_live_at_gc)) as op),
        [arg1; Cop(Cadd (sz', (Must_scan | Cannot_be_live_at_gc)), [arg2; Cconst_int n], _)], dbg)
    when sz = sz' && is_offset chunk n ->
      (Iindexed n, Cop(op, [arg1; arg2], dbg))
  | Cconst_symbol (s, _)
    when use_direct_addressing s ->
      (Ibased(s, 0), Ctuple [])
  | arg ->
      (Iindexed 0, arg)

method! select_operation op args dbg =
  match op with
  (* Integer addition *)
  | Cadd (sz, _) ->
      begin match args with
      (* Add immediate *)
      | [arg; Cconst_int n] when self#is_immediate n ->
          ((if n >= 0 then Iintop_imm(sz, Iadd, n) else Iintop_imm(sz, Isub, -n)),
           [arg])
      | [Cconst_int n; arg] when self#is_immediate n ->
          ((if n >= 0 then Iintop_imm(sz, Iadd, n) else Iintop_imm(sz, Isub, -n)),
           [arg])
      (* Shift-add *)
      | [arg1; Cop(Clsl _, [arg2; Cconst_int n], _)] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftadd, n)), [arg1; arg2])
      | [arg1; Cop(Casr _, [arg2; Cconst_int n], _)] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftadd, -n)), [arg1; arg2])
      | [Cop(Clsl _, [arg1; Cconst_int n], _); arg2] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftadd, n)), [arg2; arg1])
      | [Cop(Casr _, [arg1; Cconst_int n], _); arg2] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftadd, -n)), [arg2; arg1])
      (* Multiply-add *) (* TODO: adapt size *)
      | [arg1; Cop(Cmul _, args2, dbg)] | [Cop(Cmul _, args2, dbg); arg1] ->
          begin match self#select_operation (Cmul Cannot_scan) args2 dbg with
          | (Iintop_imm(Atarget, Ilsl, l), [arg3]) ->
              (Ispecific(Ishiftarith(Ishiftadd, l)), [arg1; arg3])
          | (Iintop (Atarget, Imul), [arg3; arg4]) ->
              (Ispecific Imuladd, [arg3; arg4; arg1])
          | _ ->
              super#select_operation op args dbg
          end
      | _ ->
          super#select_operation op args dbg
      end
  (* Integer subtraction *)
  | Csub _ -> (* TODO: adapt size *)
      begin match args with
      (* Sub immediate *)
      | [arg; Cconst_int n] when self#is_immediate n ->
          ((if n >= 0 then Iintop_imm(Atarget, Isub, n) else Iintop_imm(Atarget, Iadd, -n)),
           [arg])
      (* Shift-sub *)
      | [arg1; Cop(Clsl _, [arg2; Cconst_int n], _)] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftsub, n)), [arg1; arg2])
      | [arg1; Cop(Casr _, [arg2; Cconst_int n], _)] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftsub, -n)), [arg1; arg2])
      (* Multiply-sub *) (* TODO: adapt size *)
      | [arg1; Cop(Cmul _, args2, dbg)] ->
          begin match self#select_operation (Cmul Cannot_scan) args2 dbg with
          | (Iintop_imm(Atarget, Ilsl, l), [arg3]) ->
              (Ispecific(Ishiftarith(Ishiftsub, l)), [arg1; arg3])
          | (Iintop (Atarget, Imul), [arg3; arg4]) ->
              (Ispecific Imulsub, [arg3; arg4; arg1])
          | _ ->
              super#select_operation op args dbg
          end
      | _ ->
          super#select_operation op args dbg
      end
  (* Checkbounds *)
  | Ccheckbound ->
      begin match args with
      | [Cop(Clsr _, [arg1; Cconst_int n], _); arg2] when n > 0 && n < 64 ->
          (Ispecific(Ishiftcheckbound { shift = n; label_after_error = None; }),
            [arg1; arg2])
      | _ ->
          super#select_operation op args dbg
      end
  (* Integer multiplication *)
  (* ARM does not support immediate operands for multiplication *)
  | Cmul _ -> (* TODO: adapt size *)
      (Iintop (Atarget, Imul), args)
  | Cmulh _ ->
      (Iintop (Atarget, Imulh), args)
  (* Bitwise logical operations have a different range of immediate
     operands than the other instructions *) (* TODO: adapt size *)
  | Cand _ -> self#select_logical Atarget Iand args
  | Cor _ -> self#select_logical Atarget Ior args
  | Cxor _ -> self#select_logical Atarget Ixor args
  (* Recognize floating-point negate and multiply *)
  | Cnegf ->
      begin match args with
      | [Cop(Cmulf, args, _)] -> (Ispecific Inegmulf, args)
      | _ -> super#select_operation op args dbg
      end
  (* Recognize floating-point multiply and add/sub *)
  | Caddf ->
      begin match args with
      | [arg; Cop(Cmulf, args, _)] | [Cop(Cmulf, args, _); arg] ->
          (Ispecific Imuladdf, arg :: args)
      | _ ->
          super#select_operation op args dbg
      end
  | Csubf ->
      begin match args with
      | [arg; Cop(Cmulf, args, _)] ->
          (Ispecific Imulsubf, arg :: args)
      | [Cop(Cmulf, args, _); arg] ->
          (Ispecific Inegmulsubf, arg :: args)
      | _ ->
          super#select_operation op args dbg
      end
  (* Recognize floating-point square root *)
  | Cextcall("sqrt", _, _, _) ->
      (Ispecific Isqrtf, args)
  (* Recognize bswap instructions *)
  | Cextcall("caml_bswap16_direct", _, _, _) ->
      (Ispecific(Ibswap 16), args)
  | Cextcall("caml_int32_direct_bswap", _, _, _) ->
      (Ispecific(Ibswap 32), args)
  | Cextcall(("caml_int64_direct_bswap"|"caml_nativeint_direct_bswap"),
              _, _, _) ->
      (Ispecific (Ibswap 64), args)
  (* Other operations are regular *)
  | _ ->
      super#select_operation op args dbg

method select_logical sz op = function
  | [arg; Cconst_int n] when is_logical_immediate n ->
      (Iintop_imm(sz, op, n), [arg])
  | [Cconst_int n; arg] when is_logical_immediate n ->
      (Iintop_imm(sz, op, n), [arg])
  | args ->
      (Iintop (sz, op), args)

end

let fundecl f = (new selector)#emit_fundecl f
