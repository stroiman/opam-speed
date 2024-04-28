open Speed
open Dsl
open Speed.Domain
open Speed.Runner
open Speed.Assertions
open Utils

(* Important, if any is commented out, tests will not run *)
open! Runner_test
open! Dsl_test

let addExample ?(name = "dummy") f c = { examples= { name; f } :: c.examples };;

Dsl.register
  [
    test "is_success should return success=true when test case doesn't raise" (fun _ ->
      expect (Context.empty |> addExample passing_test |> run_suite |> is_success) be_true);
    test "Run should return success=false when test case raises" (fun _ ->
      expect
        (Context.empty |> addExample failing_test |> run_suite |> is_success)
        be_false);
  ]
;;

!Speed.Dsl.root_suite |> run_main
