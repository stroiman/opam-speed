open Speed
open Dsl
open Assertions
open Utils;;

register
  [
    test "Int assertion errors print actual and expected" (fun _ ->
      let actual =
        try
          expect 1 (equal_int 2);
          ""
        with
        | FormattedAssertionError f -> get_printed_string f
      in
      let expected = "Assertion error\n  Expected: 2\n  Actual: 1" in
      expect actual (equal_string expected));
    test "If a name is specified, it is added to the output" (fun _ ->
      let actual =
        try
          expect ~name:"The value compared" 1 (equal_int 2);
          ""
        with
        | FormattedAssertionError f -> get_printed_string f
      in
      let expected = "Assertion error: The value compared\n  Expected: 2\n  Actual: 1" in
      expect actual (equal_string expected));
  ]
