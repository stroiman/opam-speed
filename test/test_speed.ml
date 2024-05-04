(* Other tests, if any is commented out, tests will not _necessarily_ run *)
open! Effect_dsl_test
open! Assertions_test
open! Dsl_test
open! Runner_test
open! Lwt_runner_test

(* Used modules *)
open Speed
open Dsl
open Speed.Domain
open Speed.Runner
open Speed.Assertions
open Utils

let fmt = null_formatter;;

root_context "Test outcome"
  [
    test "is_success should return success=true when test case doesn't raise"
      [%f
        expect
          (Context.empty
           |> addExample passing_test
           |> run_suite ~fmt
           |> is_success
          )
          be_true];
    test "is_success should return success=false when test case raises"
      [%f_
        expect
          (Context.empty
           |> addExample failing_test
           |> run_suite ~fmt
           |> is_success
          )
          be_false];
    test "should print exception message if test throes" (fun _ ->
      let fmt, get_string = make_string_printer () in
      let suite =
        Context.empty |> addExample (fun _ -> failwith "error message from test")
      in
      let _ = run_suite ~fmt suite in
      get_string () |> should @@ contain "error message from test"
    );
  ]
;;

!Speed.Dsl.root_suite |> run_main
