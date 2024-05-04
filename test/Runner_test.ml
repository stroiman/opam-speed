open Speed
open Speed.Dsl
open Speed.Assertions
open Speed.Runner
open Utils

let fmt = null_formatter;;

root_context "Microtask runner"
  [
    context "Counting no of failing tests"
      [
        test "One failing test should result in fail count of 1" (fun _ ->
          let suite =
            Domain.Context.empty |> add_failing_example |> add_passing_example
          in
          let result = run_suite ~fmt suite |> get_no_of_failing_examples in
          expect result @@ equal_int 1
        );
        test "Two failing tests should result in fail count of 2" (fun _ ->
          let suite =
            Domain.Context.empty |> add_failing_example |> add_failing_example
          in
          let result = run_suite ~fmt suite |> get_no_of_failing_examples in
          expect result @@ equal_int 2
        );
      ];
    test "Should run the examples in the specified order" (fun _ ->
      let suite =
        parse [test "Example 1" passing_test; test "Example 2" passing_test]
      in
      let output = ref "" in
      let fmt = make_ref_string_printer output in
      run_suite ~fmt suite |> ignore;
      expect !output @@ equal_string "✔ Example 1\n✔ Example 2"
    );
    context "Running nested contexts"
      [
        test "Should run tests inside groups"
          begin
            fun _ ->
              let suite =
                Domain.Context.empty
                |> add_child_group (fun x ->
                  x |> add_passing_example |> add_passing_example
                )
                |> add_child_group (fun x ->
                  x |> add_passing_example |> add_failing_example
                )
              in
              let suite_result = run_suite ~fmt suite in
              expect (get_no_of_failing_examples suite_result) @@ equal_int 1;
              expect (get_no_of_passing_examples suite_result) @@ equal_int 3
          end;
        test "Should print the group name in output" (fun _ ->
          let suite =
            Domain.Context.empty
            |> add_child_group ~name:"Grp 1" (add_passing_example ~name:"Ex")
            |> add_child_group ~name:"Grp 2" (add_passing_example ~name:"Ex")
          in
          let output = ref "" in
          let fmt = make_ref_string_printer output in
          run_suite ~fmt suite |> ignore;
          expect !output @@ equal_string "• Grp 1\n  ✔ Ex\n• Grp 2\n  ✔ Ex"
        );
      ];
  ]
