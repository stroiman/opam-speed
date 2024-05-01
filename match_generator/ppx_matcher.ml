open Ppxlib
open Base

let[@warning "-26"] str_gen ~loc ~path:_ (_rec, t) =
  let t = List.hd_exn t in
  let (module Ast) = Ast_builder.make loc in
  let fields =
    match t.ptype_kind with
    | Ptype_record fields -> fields
    | _ -> Location.raise_errorf ~loc "tuple only works on records"
  in
  let lident_of_field field =
    Ast_builder.Default.Located.lident ~loc:field.pld_name.loc
      field.pld_name.txt
  in
  let field_names = List.map ~f:(fun f -> f.pld_name.txt) fields in
  let fun_ =
    let f_name =
      let type_name = t.ptype_name.txt in
      type_name ^ "_matcher"
    in
    let pat = Ast.pvar f_name in
    let _body = Ast.pexp_constant (Ast_helper.Const.string "Foobar") in
    let body =
      let ok_unit () =
        Ast.pexp_construct
          (Loc.make ~loc (Longident.parse "Ok"))
          (Some (Ast.pexp_construct (Loc.make ~loc (Longident.parse "()")) None))
      in
      let init = ok_unit () in
      fields
      |> List.fold_right ~init ~f:(fun field _iter ->
        Ast.pexp_apply
          (Ast.pexp_ident (Loc.make ~loc (Longident.parse "Result.bind")))
          [
            Nolabel, _iter;
            ( Nolabel,
              Ast.pexp_fun Nolabel None Ast.ppat_any
                (Ast.pexp_apply
                   (Ast.pexp_ident
                      (Loc.make ~loc (Longident.parse "Option.fold"))
                   )
                   [
                     Labelled "none", ok_unit ();
                     ( Labelled "some",
                       Ast.pexp_fun Nolabel None
                         (Ast.ppat_var (Loc.make ~loc "v"))
                         (Ast.pexp_apply
                            (Ast.pexp_ident
                               (Loc.make ~loc (Longident.parse "Result.map"))
                            )
                            [
                              ( Nolabel,
                                Ast.pexp_ident
                                  (Loc.make ~loc (Longident.parse "ignore")) );
                              ( Nolabel,
                                Ast.pexp_apply
                                  (Ast.pexp_ident
                                     (Loc.make ~loc (Longident.parse "v"))
                                  )
                                  [
                                    ( Nolabel,
                                      Ast.pexp_field
                                        (Ast.pexp_ident
                                           (Loc.make ~loc
                                              (Longident.parse "actual")
                                           )
                                        )
                                        (lident_of_field field) );
                                  ] );
                            ]
                         ) );
                     Nolabel, Ast.pexp_ident (lident_of_field field);
                   ]
                ) );
          ]
      )
    in
    let init =
      Ast.pexp_fun Nolabel None (Ast.ppat_var (Loc.make ~loc "actual")) body
    in
    let expr =
      fields
      |> List.fold_right ~init ~f:(fun name inner ->
        Ast.pexp_fun (Optional name.pld_name.txt) None
          (Ast.ppat_var name.pld_name)
          inner
      )
    in
    [Ast.value_binding ~pat ~expr] |> Ast.pstr_value Nonrecursive
  in
  [fun_]
;;

let str_type_decl = Deriving.Generator.make_noarg str_gen
let my_deriver = Deriving.add ~str_type_decl "matcher"
