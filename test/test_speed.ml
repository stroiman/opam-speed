open Speed
open! Dsl
open! Speed.Domain
open Speed.Runner
open! Speed.Assertions
open! Utils

(* Important, if any is commented out, tests will not run *)
open! Effect_dsl_test
open! Assertions_test
open! Dsl_test
open! Runner_test

let fmt = null_formatter;;

root_context
  "Test outcome"
  [
    test
      "is_success should return success=true when test case doesn't raise"
      [%f
        expect
          (Context.empty |> addExample passing_test |> run_suite ~fmt |> is_success)
          be_true];
    test
      "is_success should return success=false when test case raises"
      [%f_
        expect
          (Context.empty |> addExample failing_test |> run_suite ~fmt |> is_success)
          be_false];
  ]
;;

!Speed.Dsl.root_suite |> run_main
