(* open StdLabels *)
open Ppxlib
(* open Ppxlib.Ast_builder.Default *)

let ppx_fun_expander_args ~loc (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Ast.Pexp_construct (id, _) ->
    let (module Ast) = Ast_builder.make loc in
    let pattern = Ast.ppat_construct id (Some (Ast.pvar "x")) in
    [%expr
      Speed.Domain.Metadata_list.find_map ~f:(function
        | [%p pattern] -> Some x
        | _ -> None
        )]
  | _ -> failwith "Unknown type"
;;

let[@warning "-27"] extension =
  Extension.declare "m" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path e -> e |> ppx_fun_expander_args ~loc)
;;

let rule = Context_free.Rule.extension extension
let () = Driver.register_transformation ~rules:[rule] "ppx_metadata"
