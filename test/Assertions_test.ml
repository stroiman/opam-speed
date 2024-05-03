open Speed
open Effect_dsl
open Assertions
open Utils

let run_and_get_error_meg f =
  try
    f ();
    raise AssertionError
  with FormattedAssertionError f -> get_printed_string f
;;

let match_sexp (parser : 'a -> Base.Sexp.t) expected (actual : 'a) =
  let open Base in
  let key, matcher = expected in
  let find_value list =
    List.find_map
      ~f:(function
        | Sexp.List [Sexp.Atom x; Sexp.Atom y] ->
          if String.equal x key then Some y else None
        | _ -> None
        )
      list
  in
  (* let sexp = Base.Sexp.of_string expected; *)
  match parser actual with
  | Sexp.List l ->
    let actual = find_value l in
    ( match actual with
      | None ->
        Error (`AssertionErrorWithFormat (Stdlib.Format.dprintf "Key not found"))
      | Some c ->
        c
        |> matcher
        |> Result.map_error ~f:(function
          | `AssertionErrorWithFormat pp ->
            `AssertionErrorWithFormat
              (Stdlib.Format.dprintf "@[<v2>Object field: %s@,%t@]" key pp)
          | x -> x
          )
    )
  | _ -> Error (`AssertionErrorWithFormat (Stdlib.Format.dprintf "Not a list"))
;;

let string_of_sexp = Base.string_of_sexp
let sexp_of_string = Base.sexp_of_string

type foo = {
  a: string;
  b: string;
}
[@@deriving sexp]

let match_sexp_of_string expected =
  match_sexp Parsexp.Single.parse_string_exn expected
;;

let match_t expected = match_sexp sexp_of_foo expected;;

run_root (fun _ ->
  context "Assertion library"
    [%f
      context "Int expectations"
        [%f
          it "Should print actual and expected"
            [%f
              let actual =
                run_and_get_error_meg (fun _ -> expect 1 (equal_int 2))
              in
              let expected = "Assertion error\n  Expected: 2\n  Actual: 1" in
              expect actual (equal_string expected)];

          it "Should include a name in the error message" (fun _ ->
            let actual =
              run_and_get_error_meg
                [%f expect ~name:"The value compared" 1 (equal_int 2)]
            in
            let expected =
              "Assertion error: The value compared\n  Expected: 2\n  Actual: 1"
            in
            expect actual (equal_string expected)
          )];

      context "String assertions" (fun _ ->
        test "String comparison errors"
          [%f
            let expected = "Assertion error\n  Expected: bar\n  Actual: Foo" in
            (fun _ -> expect "Foo" (equal_string "bar"))
            |> run_and_get_error_meg
            |> should (equal_string expected)];

        context "contains" (fun _ ->
          test "Should succeed when actual contains substring" (fun _ ->
            let matcher = contain "def" in
            let actual = "abcdefgeh" in
            actual |> run_matcher matcher |> should be_ok
          );

          test "Should fail when actual contains substring" (fun _ ->
            let matcher = contain "xyz" in
            let actual = "abcdefgeh" in
            actual |> run_matcher matcher |> should be_error
          )
        )
      );

      test "Sexp comparison" (fun _ ->
        let input = "((a value_for_a)(b value_for_b))" in
        input |> should (match_sexp_of_string ("a", equal_string "value_for_a"))
      );

      test "Record sexp comparison" (fun _ ->
        let actual = { a= "Value for a"; b= "Value for b" } in
        expect actual (match_t ("a", equal_string "Value for a"))
      )]
)
