(* open StdLabels *)
open Ppxlib
(* open Ppxlib.Ast_builder.Default *)

let ppx_fun_expander_args ~loc (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Ast.Pexp_construct (id, default) ->
    let (module Ast) = Ast_builder.make loc in
    let pattern = Ast.ppat_construct id (Some (Ast.pvar "x")) in
    let lookup =
      [%expr
        Speed.Domain.Metadata_list.find_map ~f:(function
          | [%p pattern] -> Some x
          | _ -> None
          )]
    in
    ( match default with
      | None -> lookup
      | Some d ->
        [%expr
          fun x ->
            x
            |> [%e lookup]
            |> function
            | Some x -> x
            | None -> [%e d]]
    )
  | _ -> failwith "Unknown type"
;;

let ppx_fun_expander_args_2 ~loc (expr : Parsetree.expression) =
  let exp = ppx_fun_expander_args ~loc expr in
  [%expr fun x -> x |> Speed.Domain.get_metadata |> [%e exp]]
;;

let[@warning "-27"] extension =
  Extension.declare "m" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path e -> e |> ppx_fun_expander_args ~loc)
;;

let[@warning "-27"] extension_2 =
  Extension.declare "mx" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path e -> e |> ppx_fun_expander_args_2 ~loc)
;;

let rule = Context_free.Rule.extension extension
let rule_2 = Context_free.Rule.extension extension_2
let () = Driver.register_transformation ~rules:[rule; rule_2] "ppx_metadata"
