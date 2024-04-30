open Speed
open Effect_dsl
open Assertions
open Utils

let expect_assertion_error f =
  try
    f ();
    raise AssertionError
  with
  | FormattedAssertionError f -> get_printed_string f
;;

run_root (fun _ ->
  context "Assertion library" (fun _ ->
    test "Int assertion errors print actual and expected" (fun _ ->
      let actual = expect_assertion_error (fun _ -> expect 1 (equal_int 2)) in
      let expected = "Assertion error\n  Expected: 2\n  Actual: 1" in
      expect actual (equal_string expected));

    test "If a name is specified, it is added to the output" (fun _ ->
      let actual =
        expect_assertion_error (fun _ ->
          expect ~name:"The value compared" 1 (equal_int 2))
      in
      let expected = "Assertion error: The value compared\n  Expected: 2\n  Actual: 1" in
      expect actual (equal_string expected));

    test "String comparison errors" (fun _ ->
      let actual = expect_assertion_error (fun _ -> expect "Foo" (equal_string "bar")) in
      let expected = "Assertion error\n  Expected: bar\n  Actual: Foo" in
      expect actual (equal_string expected))))
