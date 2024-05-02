open Ppxlib
open Base

let str_gen ~loc ~path:_ (_rec, t) =
  let t = List.hd_exn t in
  let (module Ast) = Ast_builder.make loc in
  let fields =
    match t.ptype_kind with
    | Ptype_record fields -> Some fields
    | _ -> None
    (* Location.raise_errorf ~loc "Matcher only works on records" *)
  in
  match fields with
  | None -> []
  | Some fields ->
    let lident_of_field field =
      Ast_builder.Default.Located.lident ~loc:field.pld_name.loc
        field.pld_name.txt
    in
    let pat =
      let type_name = t.ptype_name.txt in
      Ast.pvar (type_name ^ "_matcher")
    in
    let expr =
      let body =
        let init = [%expr Ok ()] in
        fields
        |> List.fold_right ~init ~f:(fun field iter ->
          let lident_actual = Loc.make ~loc (Longident.parse "actual") in
          [%expr
            [%e iter]
            |> Speed.Assertions.AssertionResult.bind ~f:(fun _ ->
              match [%e Ast.pexp_ident (lident_of_field field)] with
              | None -> Ok ()
              | Some v ->
                [%e
                  Ast.pexp_field
                    (Ast.pexp_ident lident_actual)
                    (lident_of_field field)]
                |> v
                |> Speed.Assertions.AssertionResult.map ignore
            )]
        )
      in
      let init = [%expr fun actual -> [%e body]] in
      fields
      |> List.fold_right ~init ~f:(fun name inner ->
        Ast.pexp_fun (Optional name.pld_name.txt) None
          (Ast.ppat_var name.pld_name)
          inner
      )
    in
    [[%stri let [%p pat] = [%e expr]]]
;;

let str_type_decl = Deriving.Generator.make_noarg str_gen
let my_deriver = Deriving.add ~str_type_decl "matcher"
