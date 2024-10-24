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

open Types

open Ez_file.V1

let file file =
  let contents = EzFile.read_file file in

  let len = String.length contents in
  let line = ref 0 in
  let line_pos = ref 0 in
  let tokens = ref [] in

  let string s = STRING s in
  let num s = NUM s in
  let ident = function
    | "typedef" -> TYPEDEF
    | "struct" -> STRUCT
    | "sizeof" -> SIZEOF
    | "const" -> CONST
    | "char" -> CHAR
    | "int" -> INT
    | "long" -> LONG
    | "short" -> SHORT
    | "signed" -> SIGNED
    | "unsigned" -> UNSIGNED
    | "union" -> UNION
    | "enum" ->ENUM
    | "volatile" -> VOLATILE
    | "double" -> DOUBLE
    | "float" -> FLOAT
    | "__signed__" -> SIGNED
    | "__unsigned__" -> UNSIGNED

    | s -> IDENT s
  in
  let token = function
    | ";" -> SEMI
    | "," -> COMMA
    | "{" -> LBRACE
    | "}" -> RBRACE
    | "[" -> LBRACKET
    | "]" -> RBRACKET
    | "*" -> STAR
    | "(" -> LPAREN
    | ")" -> RPAREN
    | "+" -> PLUS
    | "-" -> MINUS
    | ":" -> COLON
    | "=" -> EQUAL

    | s -> TOKEN s
  in
  let add_token make_token pos0 pos =
    let loc = { file ; line = !line ; line_pos = !line_pos ;
                begin_pos = pos0 ; end_pos = pos } in
    let tok = String.sub contents pos0 (pos-pos0)  in
    match tok with
    | "__extension__" -> ()
    | _ ->
        let tok = make_token tok in
        (* Printf.eprintf "tok = %s\n%!" (string_of_token tok) ; *)
        tokens := (tok, loc) :: !tokens ;
  in

  let rec iter_line pos =
    incr line ;
    line_pos := pos;
    if pos < len then
      match contents.[pos] with
      | '\n' -> iter_line (pos+1)
      | '#' -> iter_eol (pos+1)
      | _ -> iter_space pos

  and iter_space pos =
    if pos < len then
      match contents.[pos] with
      | '\n' -> iter_line (pos+1)
      | ' ' | '\t' -> iter_space (pos+1)
      | '0'..'9' -> iter_num pos (pos+1)
      | 'a'..'z' | '_' | 'A'..'Z' -> iter_ident pos (pos+1)
      | '"' -> iter_string pos (pos+1)
      | '\'' when pos < len+2 &&
                  contents.[pos+1] <> '\\' &&
                  contents.[pos+2] == '\'' ->
          add_token (fun s -> CHARACTER s) pos (pos+2);
          iter_space (pos+3)
      | _ ->
          add_token token pos (pos+1);
          iter_space (pos+1)

  and iter_string pos0 pos =
    if pos < len then
      match contents.[pos] with
      | '\\' when contents.[pos+1] = '"' ->
          iter_string pos0 (pos+2)
      | '"' ->
          add_token string pos0 pos;
          iter_space (pos+1)
      | _ ->
          iter_string pos0 (pos+1)
    else
      assert false

  and iter_num pos0 pos =
    if pos < len then
      match contents.[pos] with
      | '0'..'9' | 'a'..'z' | '_' | 'A'..'Z' -> iter_num pos0 (pos+1)
      | _ ->
          add_token num pos0 pos;
          iter_space pos
    else
      add_token num pos0 pos

  and iter_ident pos0 pos =
    if pos < len then
      match contents.[pos] with
      | '0'..'9' | 'a'..'z' | '_' | 'A'..'Z' -> iter_ident pos0 (pos+1)
      | _ ->
          add_token ident pos0 pos;
          iter_space pos
    else
      add_token ident pos0 pos

  and iter_eol pos =
    if pos < len then
      match contents.[pos] with
      | '\n' -> iter_line (pos+1)
      | _ -> iter_eol (pos+1)

  in
  iter_line 0;
  contents, List.rev !tokens
