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

let canonize ~first type_decls =

  let typedefs = Hashtbl.create 1000 in

  let rec iter0 type_decls =
    match type_decls with
    | [] ->
        Printf.eprintf "Error: First public type %S not found\n%!" first;
        exit 2
    | ty :: type_decls ->
        let name =
        match ty with
          | Type_def (name, _, _) -> name
          | Struct_def (name, _) -> name
          | Enum_def (name, _) -> name
          | Union_def (name, _) -> name
        in
        if name = first then
          iter1 [] (ty :: type_decls)
        else
          iter0 type_decls

  and iter1 rev type_decls =
    match type_decls with
    | [] -> List.rev rev
    | ty :: type_decls ->
        match ty with
          | Struct_def (name, sd) ->
              let sd = { sd with
                         struct_fields = List.map iter_field sd.struct_fields
                       } in
              let ty = Struct_def (name, sd) in
              iter1 (ty :: rev) type_decls
          | Union_def (name, sd) ->
              let sd = { sd with
                         union_fields = List.map iter_field sd.union_fields
                       } in
              let ty = Union_def (name, sd) in
              iter1 (ty :: rev) type_decls
          | Enum_def _ ->
              iter1 (ty :: rev) type_decls
          | Type_def (name, _, f) ->
              let f,rev  = match f.typename with
                | Struct (Some name2, Some sd) ->
                  let f = { f with
                            typename = Struct (Some name2, None) }
                  in
                  let sd = { sd with
                             struct_fields =
                               List.map iter_field sd.struct_fields }
                  in
                  let rev = Struct_def (name2, sd) :: rev in
                  f, rev
                | Struct (None, Some sd) ->
                  let f = { f with
                            typename = Struct (Some name, None) }
                  in
                  let sd = { sd with
                             struct_fields =
                               List.map iter_field sd.struct_fields }
                  in
                  let rev = Struct_def (name, sd) :: rev in
                  f, rev
                | Union (Some name2, Some sd) ->
                  let f = { f with
                            typename = Union (Some name2, None) }
                  in
                  let sd = { sd with
                             union_fields =
                               List.map iter_field sd.union_fields }
                  in
                  let rev = Union_def (name2, sd) :: rev in
                  f, rev
                | Union (None, Some sd) ->
                  let f = { f with
                            typename = Union (Some name, None) }
                  in
                  let sd = { sd with
                             union_fields =
                               List.map iter_field sd.union_fields }
                  in
                  let rev = Union_def (name, sd) :: rev in
                  f, rev
                | _ -> f, rev
              in
              let f = iter_field f in
              assert (f.occur = One);
              Hashtbl.add typedefs name f;
              iter1 rev type_decls

  and iter_field f =
    match f.typename with
    | Type_name name ->
        begin
          match Hashtbl.find typedefs name with
          | exception Not_found ->
              Printf.eprintf "Warning: typedef %S not found\n%!" name;
              f
          | f2 ->
              { f with
                stars = f.stars + f2.stars ;
                typename = f2.typename }
        end
    | _ -> f

  in
  ignore (first);
  iter0 type_decls
