open Speed.Domain
open Speed.Runner
open Speed.Assertions

exception TestError

let passing_test () = ()
let failing_test () = raise TestError

let () =
  {
    examples =
      [
        {
          name =
            "is_success should return success=true when test case doesn't raise";
          f =
            (fun _ ->
              expect
                ({
                   examples = [ { name = "Passing example"; f = passing_test } ];
                 }
                |> run_suite |> is_success)
                be_true);
        };
        {
          name = "Run should return success=false when test case raises";
          f =
            (fun _ ->
              expect
                ({
                   examples = [ { name = "Failing example"; f = failing_test } ];
                 }
                |> run_suite |> is_success)
                be_false);
        };
      ];
  }
  |> run_main
