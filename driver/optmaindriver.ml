(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Clflags
<<<<<<< HEAD
open Compenv
=======
>>>>>>> ocaml/4.12

module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Compilenv.symbol_for_global'
<<<<<<< HEAD
=======
  let closure_symbol = Compilenv.closure_symbol

  let really_import_approx = Import_approx.really_import_approx
  let import_symbol = Import_approx.import_symbol
>>>>>>> ocaml/4.12

  let size_int = Arch.size_int
  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end
let backend = (module Backend : Backend_intf.S)

let usage = "Usage: ocamlopt <options> <files>\nOptions are:"

module Options = Main_args.Make_optcomp_options (Main_args.Default.Optmain)
let main argv ppf =
  native_code := true;
<<<<<<< HEAD
  begin match Sys.getenv_opt "FLAMBDA_COLUMNS" with
  | None -> ()
  | Some i ->
    match int_of_string i with
    | exception _ -> ()
    | i ->
      if i >= 40 then begin
        Format.pp_set_margin Format.std_formatter i;
        Format.pp_set_margin Format.err_formatter i
      end
  end;
  match
    readenv ppf Before_args;
=======
  match
    Compenv.readenv ppf Before_args;
>>>>>>> ocaml/4.12
    Clflags.add_arguments __LOC__ (Arch.command_line_options @ Options.list);
    Clflags.add_arguments __LOC__
      ["-depend", Arg.Unit Makedepend.main_from_option,
       "<options> Compute dependencies \
        (use 'ocamlopt -depend -help' for details)"];
<<<<<<< HEAD
    Clflags.parse_arguments argv anonymous usage;
    Compmisc.read_clflags_from_env ();
    if !Clflags.plugin then
      fatal "-plugin is only supported up to OCaml 4.08.0";
=======
    Clflags.parse_arguments argv Compenv.anonymous usage;
    Compmisc.read_clflags_from_env ();
    if !Clflags.plugin then
      Compenv.fatal "-plugin is only supported up to OCaml 4.08.0";
>>>>>>> ocaml/4.12
    begin try
      Compenv.process_deferred_actions
        (ppf,
         Optcompile.implementation ~backend,
         Optcompile.interface,
         ".cmx",
         ".cmxa");
    with Arg.Bad msg ->
      begin
        prerr_endline msg;
        Clflags.print_arguments usage;
        exit 2
      end
    end;
<<<<<<< HEAD
    readenv ppf Before_link;
    if
      List.length (List.filter (fun x -> !x)
                     [make_package; make_archive; shared;
                      stop_early; output_c_object]) > 1
=======
    Compenv.readenv ppf Before_link;
    if
      List.length (List.filter (fun x -> !x)
                     [make_package; make_archive; shared;
                      Compenv.stop_early; output_c_object]) > 1
>>>>>>> ocaml/4.12
    then
    begin
      let module P = Clflags.Compiler_pass in
      match !stop_after with
      | None ->
<<<<<<< HEAD
        fatal "Please specify at most one of -pack, -a, -shared, -c, \
             -output-obj";
      | Some ((P.Parsing | P.Typing | P.Scheduling) as p) ->
        assert (P.is_compilation_pass p);
        Printf.ksprintf fatal
          "Options -i and -stop-after (%s) \
           are  incompatible with -pack, -a, -shared, -output-obj"
          (String.concat "|"
             (Clflags.Compiler_pass.available_pass_names ~native:true))
    end;
    if !make_archive then begin
      Compmisc.init_path ();
      let target = extract_output !output_name in
      Asmlibrarian.create_archive
        (get_objfiles ~with_ocamlparam:false) target;
=======
          Compenv.fatal "Please specify at most one of -pack, -a, -shared, -c, \
                         -output-obj";
      | Some ((P.Parsing | P.Typing | P.Scheduling | P.Emit) as p) ->
        assert (P.is_compilation_pass p);
        Printf.ksprintf Compenv.fatal
          "Options -i and -stop-after (%s) \
           are  incompatible with -pack, -a, -shared, -output-obj"
          (String.concat "|"
             (P.available_pass_names ~filter:(fun _ -> true) ~native:true))
    end;
    if !make_archive then begin
      Compmisc.init_path ();
      let target = Compenv.extract_output !output_name in
      Asmlibrarian.create_archive
        (Compenv.get_objfiles ~with_ocamlparam:false) target;
>>>>>>> ocaml/4.12
      Warnings.check_fatal ();
    end
    else if !make_package then begin
      Compmisc.init_path ();
<<<<<<< HEAD
      let target = extract_output !output_name in
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
        Asmpackager.package_files ~ppf_dump (Compmisc.initial_env ())
          (get_objfiles ~with_ocamlparam:false) target ~backend);
=======
      let target = Compenv.extract_output !output_name in
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
        Asmpackager.package_files ~ppf_dump (Compmisc.initial_env ())
          (Compenv.get_objfiles ~with_ocamlparam:false) target ~backend);
>>>>>>> ocaml/4.12
      Warnings.check_fatal ();
    end
    else if !shared then begin
      Compmisc.init_path ();
<<<<<<< HEAD
      let target = extract_output !output_name in
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
        Asmlink.link_shared ~ppf_dump
          (get_objfiles ~with_ocamlparam:false) target);
      Warnings.check_fatal ();
    end
    else if not !stop_early && !objfiles <> [] then begin
      let target =
        if !output_c_object then
          let s = extract_output !output_name in
=======
      let target = Compenv.extract_output !output_name in
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
        Asmlink.link_shared ~ppf_dump
          (Compenv.get_objfiles ~with_ocamlparam:false) target);
      Warnings.check_fatal ();
    end
    else if not !Compenv.stop_early && !objfiles <> [] then begin
      let target =
        if !output_c_object then
          let s = Compenv.extract_output !output_name in
>>>>>>> ocaml/4.12
          if (Filename.check_suffix s Config.ext_obj
            || Filename.check_suffix s Config.ext_dll)
          then s
          else
<<<<<<< HEAD
            fatal
=======
            Compenv.fatal
>>>>>>> ocaml/4.12
              (Printf.sprintf
                 "The extension of the output file must be %s or %s"
                 Config.ext_obj Config.ext_dll
              )
        else
<<<<<<< HEAD
          default_output !output_name
      in
      Compmisc.init_path ();
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
        Asmlink.link ~ppf_dump (get_objfiles ~with_ocamlparam:true) target);
=======
          Compenv.default_output !output_name
      in
      Compmisc.init_path ();
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
          let objs = Compenv.get_objfiles ~with_ocamlparam:true in
          Asmlink.link ~ppf_dump objs target);
>>>>>>> ocaml/4.12
      Warnings.check_fatal ();
    end;
  with
  | exception (Compenv.Exit_with_status n) ->
    n
  | exception x ->
    Location.report_exception ppf x;
    2
  | () ->
    Profile.print Format.std_formatter !Clflags.profile_columns;
    0
