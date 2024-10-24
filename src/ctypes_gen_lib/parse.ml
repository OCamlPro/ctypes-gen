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

let int_of_string s =
  match s with
  | "1U" -> 1
  | "2U" -> 2
  | "3U" -> 3
  | _ ->
      try
        int_of_string s
      with
      | _ ->
          Printf.kprintf failwith "int_of_string(%S)" s

let error ~contents ~tokens fmt =
  Printf.kprintf (fun s ->
      begin
        match tokens with
        | [] ->
            Printf.eprintf "Error at end of file:\n%s\n%!" s
        | (_,  loc) :: _ ->
            Printf.eprintf "Error in %s:%d (char %d-%d):\n%s\n%!"
              loc.file loc.line loc.begin_pos loc.end_pos s;
            let len = String.length contents in
            let maxpos = min len (loc.line_pos + 200) in
            let s = String.sub contents loc.line_pos (maxpos - loc.line_pos) in
            let lines = EzString.split s '\n' in
            Printf.eprintf "> %s\n%!"
              ( String.concat "\n> " lines )
      end;
      let rec iter num tokens =
        if num = 0 then () else
          match tokens with
          | [] -> ()
          | (tok, _) :: tokens ->
              Printf.eprintf "Token %s\n%!" (string_of_token tok);
              iter (num-1) tokens
      in
      iter 5 tokens;
      exit 2
    ) fmt


let parse ~contents tokens =

  let typedecls = ref [] in
  let ids = ref 0 in

  let struct_def ~loc ?name fields =
    let struct_id = !ids in
    incr ids ;
    let s = {
      struct_name = name ;
      struct_loc = loc ;
      struct_id ;
      struct_fields = fields ;
    }
    in
    begin
      match name with
      | None -> ()
      | Some name ->
          typedecls := (Struct_def (name, s)) :: !typedecls ;
    end;
    s
  in

  let union_def ~loc ?name fields =
    let union_id = !ids in
    incr ids ;
    let u = {
      union_name = name ;
      union_loc = loc ;
      union_id ;
      union_fields = fields ;
    }
    in
    begin
      match name with
      | None -> ()
      | Some name ->
          typedecls := (Union_def (name, u)) :: !typedecls
    end;
    u
  in

  let enum_def ~loc ?name fields =
    let enum_id = !ids in
    incr ids ;
    let u = {
      enum_name = name ;
      enum_loc = loc ;
      enum_id ;
      enum_fields = fields ;
    }
    in
    begin
      match name with
      | None -> ()
      | Some name ->
          typedecls := (Enum_def (name, u)) :: !typedecls
    end;
    u
  in

  let rec clean rev tokens =
    match tokens with
    | [] -> List.rev rev
    | (IDENT "__attribute__", _) :: (LPAREN,_) :: tokens ->
        let tokens = skip_to_rparen 0 tokens in
        clean rev tokens
    | tok :: tokens ->
        clean ( tok :: rev ) tokens

  and skip_to_rparen level tokens =
    (*
    begin
      match tokens with
      | [] -> ()
      | (tok,_) :: _ ->
          Printf.eprintf "skip_to_rparen %S\n%!"
            (string_of_token tok)
    end;
*)
    match tokens with
    | (RPAREN, _ ) :: tokens ->
        if level > 0 then
          skip_to_rparen (level-1) tokens
        else
          tokens
    | (LPAREN, _ ) :: tokens ->
        skip_to_rparen (level+1) tokens
    | [] | (SEMI,_) :: _ ->
        error ~contents ~tokens "skip_to_rparen"
    | _ :: tokens ->
        skip_to_rparen level tokens
  in

  let tokens = clean [] tokens in


  let rec iter tokens =
    match tokens with
    | [] -> ()
    | (ENUM, loc ) :: (IDENT name, _ ) :: (LBRACE, _) :: tokens ->
        let fields, tokens = iter_enum [] tokens in
        ignore ( enum_def ~loc ~name fields );
        iter tokens
    | (STRUCT, loc ) :: (IDENT name, _ ) :: (LBRACE, _) :: tokens ->
        let fields, tokens = iter_struct [] tokens in
        ignore ( struct_def ~loc ~name fields ) ;
        iter tokens
    | (UNION, loc ) :: (IDENT name, _ ) :: (LBRACE, _) :: tokens ->
        let fields, tokens = iter_struct [] tokens in
        ignore ( union_def ~loc ~name fields ) ;
        iter tokens
    | (TYPEDEF, _) :: tokens ->
        let field, next_tokens = iter_field tokens in
        let name = match field.name with
          | None -> error ~tokens ~contents "missing typedef name"
          | Some name -> name
        in
        let type_id = !ids in
        incr ids ;
        typedecls := (Type_def (name, type_id, field)) :: !typedecls ;
        iter next_tokens
    | _ :: tokens -> iter tokens

  and iter_enum fields tokens =
    match tokens with
    | (RBRACE, _) :: tokens -> List.rev fields, tokens
    | (IDENT name, _) :: (EQUAL, _) :: (NUM value, _) :: tokens ->
        iter_enum ( (name, Some value) :: fields) tokens
    | (IDENT name, _) :: (EQUAL, _) :: (CHARACTER value, _) :: tokens ->
        iter_enum ( (name, Some value) :: fields) tokens
    | (IDENT name, _) :: tokens ->
        iter_enum ( (name, None) :: fields) tokens
    | (COMMA, _) :: tokens -> iter_enum fields tokens
    | _ ->
        error ~contents ~tokens "iter_enum"

  and iter_occur expr tokens =
    match tokens with
    | [] -> error ~contents ~tokens "iter_occur"
    | (RBRACKET, _) :: (SEMI, _ ) :: tokens -> Array ( List.rev expr ), tokens
    | (tok, _) :: tokens ->
        iter_occur (tok :: expr) tokens

(*
    | (NUM _ as tok, _) :: tokens ->
        iter_occur (tok :: expr) tokens
    | (SIZEOF, _) :: (LPAREN, _) :: (IDENT name, _) :: (RPAREN, _) :: tokens ->
        iter_occur (IDENT name :: SIZEOF :: expr) tokens
    | ( (PLUS|MINUS|STAR) as tok, _) :: tokens ->
        iter_occur (tok :: expr) tokens
    | _ ->
        error ~contents ~tokens "iter_occur"
*)

  and skip_to_rparen level tokens =
    (*
    begin
      match tokens with
      | [] -> ()
      | (tok,_) :: _ ->
          Printf.eprintf "skip_to_rparen %S\n%!"
            (string_of_token tok)
    end;
*)
    match tokens with
    | (RPAREN, _ ) :: tokens ->
        if level > 0 then
          skip_to_rparen (level-1) tokens
        else
          tokens
    | (LPAREN, _ ) :: tokens ->
        skip_to_rparen (level+1) tokens
    | [] | (SEMI,_) :: _ ->
        error ~contents ~tokens "skip_to_rparen"
    | _ :: tokens ->
        skip_to_rparen level tokens

  and iter_struct fields tokens =
    match tokens with
    | (RBRACE, _) :: tokens -> List.rev fields, tokens
    | _ ->
        let field, tokens = iter_field tokens in
        iter_struct ( field :: fields ) tokens

  and iter_field tokens =
    let typename, loc, tokens = iter_type tokens in
    let stars, tokens = iter_stars 0 tokens in

    match tokens with
    | (LPAREN,_) :: (STAR, _) :: tokens ->
        (* function type *)
        let name, tokens =
          match tokens with
          | (IDENT name, _) :: (RPAREN, _) :: (LPAREN, _) :: tokens ->
              Some name, tokens
          | (RPAREN, _) :: (LPAREN, _) :: tokens ->
              None, tokens
          | _ ->
              error ~tokens ~contents "expecting )("
        in
        let tokens = skip_to_rparen 0 tokens in
        let tokens =
          match tokens with
          | (SEMI, _) :: tokens -> tokens
          | _ ->
              error ~contents ~tokens "iter_struct function error"
        in
        { typename = Function ;
          loc ;
          stars = 1 ;
          name ;
          occur = One }, tokens
    | _ ->
        let name, tokens =
          match tokens with
          | (IDENT name, _) :: tokens -> Some name, tokens
          | _ -> None, tokens
        in
        match tokens with
        | (SEMI, _) :: tokens ->
            let occur = One in
            { typename ; loc ; stars ; name ; occur }, tokens
        | (LBRACKET, _) :: tokens ->
            let occur, tokens = iter_occur [] tokens in
            { typename ; loc ; stars ; name ; occur }, tokens
        | (COLON, _) :: (NUM num, _) :: (SEMI, _) :: tokens ->
            let occur = Bits (int_of_string num) in
            { typename ; loc ; stars ; name ; occur }, tokens
        | (LPAREN, _) :: tokens ->
            let tokens = skip_to_rparen 0 tokens in
            let tokens =
              match tokens with
              | (SEMI, _) :: tokens -> tokens
              | _ ->
                  error ~contents ~tokens "iter_struct occur function error"
            in
            { typename = Function ;
              loc ;
              stars = 0 ;
              name ;
              occur = One }, tokens
        | _ ->
            error ~contents ~tokens "iter_struct occur error"

  and iter_integer loc attrs tokens =
    match tokens with
    | (CONST, _) :: tokens -> iter_integer loc attrs tokens
    | (VOLATILE, _ ) :: tokens -> iter_integer loc attrs tokens

    | (DOUBLE, _ ) :: tokens -> iter_integer loc (Double :: attrs ) tokens
    | (FLOAT, _ ) :: tokens -> iter_integer loc (Float :: attrs ) tokens
    | (CHAR, _ ) :: tokens -> iter_integer loc (Char :: attrs ) tokens
    | (SHORT, _ ) :: tokens -> iter_integer loc (Short :: attrs ) tokens
    | (INT, _ ) :: tokens -> iter_integer loc (Int :: attrs ) tokens
    | (LONG, _ ) :: tokens -> iter_integer loc (Long :: attrs ) tokens
    | (SIGNED, _ ) :: tokens -> iter_integer loc (Signed :: attrs ) tokens
    | (UNSIGNED, _ ) :: tokens -> iter_integer loc (Unsigned :: attrs ) tokens
    | _ ->
        let attrs = List.sort compare attrs in
        Integer attrs, loc, tokens

  and iter_type tokens =
    match tokens with
    | (CONST, _) :: tokens -> iter_type tokens
    | ( ( VOLATILE | CHAR | SHORT | INT
        | LONG | SIGNED | UNSIGNED
        | DOUBLE | FLOAT) , loc ) :: _ -> iter_integer loc [] tokens
    | (IDENT name, loc) :: tokens ->
        Type_name name, loc, tokens

    | (STRUCT, loc) :: (IDENT name, _) :: (LBRACE, _) :: tokens ->
        let fields, tokens = iter_struct [] tokens in
        let s = struct_def ~loc ~name fields in
        Struct (Some name, Some s), loc, tokens
    | (STRUCT, loc) :: (LBRACE, _) :: tokens ->
        let fields, tokens = iter_struct [] tokens in
        let s = struct_def ~loc fields in
        Struct (None, Some s), loc, tokens
    | (STRUCT, loc) :: (IDENT name, _) :: tokens ->
        Struct (Some name, None), loc, tokens

    | (UNION, loc) :: (IDENT name, _) :: (LBRACE, _) :: tokens ->
        let fields, tokens = iter_struct [] tokens in
        let u = union_def ~name ~loc fields in
        Union (Some name, Some u), loc, tokens
    | (UNION, loc) :: (IDENT name, _) :: tokens ->
        Union (Some name, None), loc, tokens
    | (UNION, loc) :: (LBRACE, _) :: tokens ->
        let fields, tokens = iter_struct [] tokens in
        let u = union_def ~loc fields in
        Union (None, Some u), loc, tokens

    | (ENUM, loc) :: (IDENT name, _) :: tokens ->
        Enum_name name, loc, tokens

    | _ ->
        error ~contents ~tokens "iter_type error"

  and iter_stars nstars tokens =
    match tokens with
    | (CONST, _) :: tokens -> iter_stars nstars tokens
    | (STAR, _) :: tokens ->
        iter_stars (nstars+1) tokens
    | _ -> nstars, tokens

  in
  iter tokens ;
  List.rev !typedecls

let file ~incl_dirs ~filename:file =
  let basename = Filename.basename file in
  let newfile = Filename.concat "/tmp" basename in
  let incl_arg = String.concat " -I " incl_dirs in
  let cmd = Printf.sprintf "cpp -I %s %s > %s" incl_arg file newfile in
  let retcode = Sys.command cmd in
  if retcode <> 0 then begin
    Printf.eprintf "Command returned error exit code %d:\n%s\n" retcode cmd;
    exit 2;
  end;

  let contents, tokens = Tokenize.file newfile in

  parse ~contents tokens
