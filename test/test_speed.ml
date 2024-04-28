open Speed
open! Dsl
open! Speed.Domain
open Speed.Runner
open! Speed.Assertions
open! Utils

(* Important, if any is commented out, tests will not run *)
open! Runner_test
open! Dsl_test
open! Assertions_test

let fmt = null_formatter;;

Dsl.register
  [
    test "is_success should return success=true when test case doesn't raise" (fun _ ->
      expect
        (Context.empty |> addExample passing_test |> run_suite ~fmt |> is_success)
        be_true);
    test "is_success should return success=false when test case raises" (fun _ ->
      expect
        (Context.empty |> addExample failing_test |> run_suite ~fmt |> is_success)
        be_false);
  ]
;;

!Speed.Dsl.root_suite |> run_main
