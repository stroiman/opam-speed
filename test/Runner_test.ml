open Speed
open Speed.Dsl
open Speed.Assertions
open Speed.Runner
open Utils

let fmt = null_formatter;;

Dsl.register
  [
    test "One failing test should result in fail count of 1" (fun _ ->
      let suite = Domain.Context.empty |> add_failing_example |> add_passing_example in
      let result = run_suite ~fmt suite |> get_no_of_failing_examples in
      expect result @@ equal_int 1);
    test "Two failing tests should result in fail count of 2" (fun _ ->
      let suite = Domain.Context.empty |> add_failing_example |> add_failing_example in
      let result = run_suite ~fmt suite |> get_no_of_failing_examples in
      expect result @@ equal_int 2);
  ]
