open Speed
open Domain
open Dsl.List
open Assertions
open Utils;;

root_context "Effect DSL"
  [
    it "Should parse a two tests"
      begin
        fun _ ->
          let suite =
            Dsl.Effect.Simple.(
              parse (fun () ->
                test "dummy" passing_test;
                test "dummy" passing_test
              )
            )
          in
          suite |> get_example_count |> should (equal_int 2)
      end;
    it "Should parse a test with context"
      begin
        fun _ ->
          let suite =
            Dsl.Effect.Simple.(
              parse (fun () ->
                context "A" (fun _ ->
                  test "a1" passing_test;
                  test "a2" passing_test
                );
                context "B" (fun _ -> test "b1" passing_test)
              )
            )
          in
          suite |> child_group_count |> should (equal_int 2);
          suite |> get_example_count |> should @@ equal_int 3
      end;
  ]
