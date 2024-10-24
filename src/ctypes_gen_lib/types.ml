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

(* Limitations:
  * we don't care about CONST annotations
*)

type token =
  | NUM of string
  | STRING of string
  | CHARACTER of string

  | IDENT of string
  | TYPEDEF
  | STRUCT
  | SIZEOF
  | CONST
  | CHAR
  | INT
  | LONG
  | SHORT
  | SIGNED
  | UNSIGNED
  | UNION
  | ENUM
  | VOLATILE
  | DOUBLE
  | FLOAT

  | TOKEN of string
  | SEMI
  | COMMA
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | STAR
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | COLON
  | EQUAL

type loc = {
  file : string ;
  line: int ;
  line_pos : int ;
  begin_pos : int ;
  end_pos : int ;
}

type attr =
  | Short
  | Int
  | Long
  | Char
  | Signed
  | Unsigned
  | Double
  | Float

type occur =
  | One
  | Array of token list
  | Bits of int

type typename =
  | Type_name of string
  | Struct of string option * struct_def option
  | Enum_name of string
  | Union of string option * union_def option
  | Integer of attr list
  | Function

and field = {
  typename : typename ;
  loc : loc ;
  stars : int ;
  occur : occur ;
  name : string option ;
}

and struct_def = {
  struct_name : string option ;
  struct_loc : loc ;
  struct_id : int ;
  struct_fields : field list ;
}

and union_def = {
  union_name : string option ;
  union_loc : loc ;
  union_id : int ;
  union_fields : field list ;
}

and enum_def = {
  enum_name : string option ;
  enum_loc : loc ;
  enum_id : int ;
  enum_fields : (string * string option ) list ;
}

and type_decl =
  | Struct_def of string * struct_def
  | Union_def of string * union_def
  | Enum_def of string * enum_def
  | Type_def of string * int * field

type kind =
  | KStruct
  | KUnion
  | KEnum
  | KTypedef




let string_of_kind = function
  | KStruct -> "struct"
  | KUnion -> "union"
  | KEnum -> "enum"
  | KTypedef -> "typedef"

let string_of_attr = function
  | Short -> "short"
  | Int -> "int"
  | Long -> "long"
  | Char -> "char"
  | Signed -> "signed"
  | Unsigned -> "unsigned"
  | Double -> "double"
  | Float -> "float"

let string_of_token = function
  | NUM s -> Printf.sprintf "NUM %S" s
  | STRING s -> Printf.sprintf "STRING %S" s
  | CHARACTER s -> Printf.sprintf "CHARACTER %s" s

  | IDENT s -> Printf.sprintf "IDENT %S" s
  | TYPEDEF -> "TYPEDEF"
  | STRUCT -> "STRUCT"
  | SIZEOF -> "SIZEOF"
  | CONST -> "CONST"
  | CHAR -> "CHAR"
  | INT -> "INT"
  | LONG -> "LONG"
  | SHORT -> "SHORT"
  | SIGNED -> "SIGNED"
  | UNSIGNED -> "UNSIGNED"
  | UNION -> "UNION"
  | ENUM -> "ENUM"
  | VOLATILE -> "VOLATILE"
  | DOUBLE -> "DOUBLE"
  | FLOAT -> "FLOAT"

  | TOKEN s -> Printf.sprintf "TOKEN %S" s
  | SEMI -> "SEMI"
  | COMMA -> "COMMA"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | STAR -> "STAR"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | COLON -> "COLON"
  | EQUAL -> "EQUAL"

let string_of_occur = function
  | One -> "One"
  | Array list ->
      Printf.sprintf "Array[%s]"
        ( String.concat " " (List.map string_of_token list))
  | Bits n -> Printf.sprintf "Bits(%d)" n
