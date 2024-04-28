open Speed.Domain
open Speed.Dsl
open Speed.Assertions
open! Utils

let no_of_examples ctx = ctx.examples |> List.length;;

register
  [
    {
      name= "Should create context with two examples";
      f=
        (fun _ ->
          expect
            ([ test "test1" passing_test; test "test2" failing_test ]
             |> parse
             |> no_of_examples)
          @@ equal_int 2);
    };
  ]