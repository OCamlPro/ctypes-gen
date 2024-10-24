(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Ez_file.V1

(* We should "canonize" the types:
   * all types not defined by the user should be seen as abstract
     easy: there is an id that makes the difference between them
   * all typedefs defined by the user should be eliminated
*)

let contents = {|

#include "gettext.h"
#include "cobc.h"
#include "tree.h"

|}

let main () =
  let incl_dirs = ref [ "." ] in
  let include_printer = ref None in
  let process () =
    let filename = "test.c" in
    Printf.eprintf "Creating file %S\n%!" filename;
    EzFile.write_file filename contents ;
    let type_decls = Parse.file ~filename
        ~incl_dirs:!incl_dirs
    in
    let type_decls = Canonize.canonize ~first:"cob_fatal_error" type_decls in
    let table = Gen_printer.table_of_types type_decls in
    let s = Gen_printer.gen_printer ?include_printer:!include_printer
        table type_decls in
    Printf.printf "%s\n%!" s
  in
  let usage = Printf.sprintf "%s [OPTIONS]" Sys.argv.(0) in
  Arg.parse [
    "-I", Arg.String (fun dir -> incl_dirs := !incl_dirs @ [dir]),
    "<DIR> Add directory to include path";
    "--generic", Arg.Unit (fun () ->
        include_printer := Some Gen_printer.Generic),
    " Generate a full generic printer";
    "--gnucobol", Arg.Unit (fun () ->
        include_printer := Some Gen_printer.GnuCOBOL),
    " Generate a printer for GnuCOBOL AST";
  ] (fun _ -> assert false) usage;
  process ()
