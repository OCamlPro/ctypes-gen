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

val table_of_types :
  Types.type_decl list ->
  (Types.kind * string, Types.type_decl) Hashtbl.t

type printer =
  | Generic
  | GnuCOBOL

val gen_printer :
  ?include_printer:printer ->
  (Types.kind * string, Types.type_decl) Hashtbl.t ->
  Types.type_decl list -> string
