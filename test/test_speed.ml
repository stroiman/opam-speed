(* Other tests, if any is commented out, tests will not _necessarily_ run *)
open! Effect_dsl_test
open! Assertions_test
open! Dsl_test
open! Runner_test
open! Lwt_runner_test
open! Focus_test

(* Used modules *)
open Speed
open Dsl
open Speed.Domain
open Speed.Runner
open Speed.Assertions
open Utils
open Null_formatter;;

root_context "Test outcome"
  [
    test "is_success should return success=true when test case doesn't raise"
      [%f
        expect
          (empty |> addExample passing_test |> run_suite ~fmt |> is_success)
          be_true];
    test "is_success should return success=false when test case raises"
      [%f_
        expect
          (empty |> addExample failing_test |> run_suite ~fmt |> is_success)
          be_false];
    test "should print exception message if test throes" (fun _ ->
      let fmt, get_string = make_string_printer () in
      let suite =
        empty |> addExample (fun _ -> failwith "error message from test")
      in
      let _ = run_suite ~fmt suite in
      get_string () |> should @@ contain "error message from test"
    );
    test "Should run both sync and lwt suites" (fun _ ->
      let test1_executed = ref false in
      let test2_executed = ref false in
      let lwt_suite =
        Domain.LwtDomain.(
          empty
          |> add_example "1" (fun _ ->
            test1_executed := true;
            Lwt.return ()
          )
        )
      in
      let sync_suite =
        Domain.Sync.(
          empty
          |> add_example "2" (fun _ ->
            test2_executed := true;
            ()
          )
        )
      in
      let result = run_suites ~fmt sync_suite lwt_suite in
      !test1_executed |> should ~name:"Test 1" be_true;
      !test2_executed |> should ~name:"Test 2" be_true;
      result.no_of_passing_examples |> should (equal_int 2)
    );
  ]
;;

run_root_suites ()
