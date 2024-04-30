open Speed
open Domain
open Dsl
open Assertions
open Utils;;

Speed.Dsl.root_context "Effect DSL"
  [
    it "Should parse a two tests"
      begin
        fun _ ->
          let suite =
            Effect_dsl.(
              parse (fun () ->
                test "dummy" passing_test;
                test "dummy" passing_test
              )
            )
          in
          suite.examples |> List.length |> should (equal_int 2)
      end;
    it "Should parse a test with context"
      begin
        fun _ ->
          let suite =
            Effect_dsl.(
              parse (fun () ->
                context "A" (fun _ ->
                  test "a1" passing_test;
                  test "a2" passing_test
                );
                context "B" (fun _ -> test "b1" passing_test)
              )
            )
          in
          suite.child_groups |> List.length |> should (equal_int 2);
          let total_no_of_examples =
            suite.child_groups
            |> List.fold_left (fun acc grp -> List.length grp.examples + acc) 0
          in
          total_no_of_examples |> should (equal_int 3)
      end;
  ]
