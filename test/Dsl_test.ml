open Speed.Domain
open Speed.Dsl.List
open Speed.Assertions
open! Utils;;

register
  [
    context "DSL parser"
      [
        test "Should create context with two examples" (fun _ ->
          expect
            ([test "test1" passing_test; test "test2" failing_test]
             |> parse
             |> get_example_count
            )
          @@ equal_int 2
        );
      ];
  ]
