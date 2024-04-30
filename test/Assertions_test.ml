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

run_root (fun _ ->
  context "Assertion library"
    [%f
      context "Int expectations"
        [%f
          it "Should print actual and expected"
            [%f
              let actual = run_and_get_error_meg (fun _ -> expect 1 (equal_int 2)) in
              let expected = "Assertion error\n  Expected: 2\n  Actual: 1" in
              expect actual (equal_string expected)];

          it "Should include a name in the error message" (fun _ ->
            let actual =
              run_and_get_error_meg [%f expect ~name:"The value compared" 1 (equal_int 2)]
            in
            let expected =
              "Assertion error: The value compared\n  Expected: 2\n  Actual: 1"
            in
            expect actual (equal_string expected)
          )];

      test "String comparison errors"
        [%f
          let expected = "Assertion error\n  Expected: bar\n  Actual: Foo" in
          (fun _ -> expect "Foo" (equal_string "bar"))
          |> run_and_get_error_meg
          |> should (equal_string expected)]]
)
