open Ppxlib
(* open StdLabels *)
(* open Ppxlib.Ast_builder.Default *)

let ppx_fun_expander_args ~loc (expr : Parsetree.expression) =
  let (module Builder) = Ast_builder.make loc in
  match expr.pexp_desc with
  | Ast.Pexp_construct (id, default) ->
    let pattern = Builder.ppat_construct id (Some (Builder.pvar "x")) in
    let lookup =
      [%expr
        Speed.Metadata.List.find_map ~f:(function
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
  | _ ->
    failwith "Bad input to ppx_metadata. Value must be a variant constructor"
;;

let ppx_fun_expander_args_2 ~loc (expr : Parsetree.expression) =
  let expr = ppx_fun_expander_args ~loc expr in
  [%expr fun x -> x |> Speed.Domain.TestInput.get_metadata |> [%e expr]]
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
