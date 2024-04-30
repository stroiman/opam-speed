(*
   The following code was extracted and adapted from ppx_fun
   - Original source: https://github.com/little-arhat/ppx_fun

   ppx_fun did not install under OCaml5. Ppx_core (which was deprecated) has been
   replaced with Ppxlib, and some code could be simplified in the process.
*)
open StdLabels
open Ppxlib
open Ppxlib.Ast_builder.Default

[@@@metaloc loc]

type placeholder =
  | Anonymous
  | Numbered of int

type context = {
  used: int list;
  highest: placeholder option;
}

let parse_placeholder s =
  if s = "__"
  then Some Anonymous
  else (
    try Some (Scanf.sscanf s "_%d" (fun i -> Numbered i)) with
    | Scanf.Scan_failure _ -> None)
;;

let set_add l el = if List.mem el ~set:l then l else el :: l

let fold_downto ~init ~f start finish =
  let rec aux counter current =
    if counter < finish then current else aux (counter - 1) (f current counter)
  in
  if start < finish
  then raise (Invalid_argument "start cannot be less then finish!")
  else aux start init
;;

let replace_and_count_placeholders prefix =
  object (_self)
    inherit [context] Ast_traverse.fold_map as super

    method! expression e acc =
      let e', acc' = super#expression e acc in
      match e'.pexp_desc with
      | Pexp_ident { txt= Longident.Lident s; loc } ->
        let maybe_placeholder = parse_placeholder s in
        (match maybe_placeholder, acc' with
         | Some Anonymous, { highest= Some Anonymous; _ } -> evar ~loc prefix, acc'
         | Some Anonymous, { highest= None; _ } ->
           evar ~loc prefix, { acc' with highest= Some Anonymous }
         | Some Anonymous, { highest= Some (Numbered _); _ } ->
           Location.raise_errorf
             ~loc
             "ppx_fun: can't use anonymous and numbered placeholders in the same \
              expression!"
         | Some (Numbered _), { highest= Some Anonymous; _ } ->
           Location.raise_errorf
             ~loc
             "ppx_fun: can't use anonymous and numbered placeholders in the same \
              expression!"
         | Some (Numbered current), { highest= Some (Numbered highest); used } ->
           let name = prefix ^ string_of_int current in
           let e = evar ~loc name in
           let new_highest =
             if current > highest then Numbered current else Numbered highest
           in
           e, { used= set_add used current; highest= Some new_highest }
         | Some (Numbered current), { used; _ } ->
           let name = prefix ^ string_of_int current in
           let e = evar ~loc name in
           e, { used= set_add used current; highest= Some (Numbered current) }
         | None, _ -> e', acc')
      | _ -> e', acc'
  end
;;

let replace_and_count_placeholders_in_expr prefix expr =
  let mapper = replace_and_count_placeholders prefix in
  let init = { used= []; highest= None } in
  mapper#expression expr init
;;

let ppx_fun_expander_args ~loc (expr : Parsetree.expression) =
  let line = loc.Location.loc_start.Lexing.pos_lnum in
  let prefix = Printf.sprintf "l_%d_v" line in
  let inner, context = replace_and_count_placeholders_in_expr prefix expr in
  match context.highest with
  | None -> [%expr fun () -> [%e inner]]
  | Some Anonymous ->
    let pat = pvar ~loc prefix in
    [%expr fun [%p pat] -> [%e inner]]
  | Some (Numbered highest) ->
    fold_downto highest 1 ~init:inner ~f:(fun exp num ->
      let name = prefix ^ string_of_int num in
      let name' = if List.mem num ~set:context.used then name else "_" ^ name in
      let pat = pvar ~loc name' in
      [%expr fun [%p pat] -> [%e exp]])
;;

let ppx_fun_expander_drop ~loc (expr : Parsetree.expression) = [%expr fun _ -> [%e expr]]

let[@warning "-27"] extension_drop =
  Extension.declare
    "f_"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path e -> e |> ppx_fun_expander_drop ~loc)
;;

let[@warning "-27"] extension_arg =
  Extension.declare
    "f"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path e -> e |> ppx_fun_expander_args ~loc)
;;

let rule1 = Context_free.Rule.extension extension_arg
let rule2 = Context_free.Rule.extension extension_drop
let () = Driver.register_transformation ~rules:[ rule1; rule2 ] "ppx_fun"
