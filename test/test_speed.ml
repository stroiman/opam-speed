open Speed.Domain
open Speed.Runner
open Speed.Assertions
open Utils

let passing_test () = ()
let failing_test () = raise TestError
let addExample ?(name = "dummy") f c = { examples = { name; f } :: c.examples }

let () =
  {
    examples =
      Dsl_test.examples
      @ [
          {
            name =
              "is_success should return success=true when test case doesn't \
               raise";
            f =
              (fun _ ->
                expect
                  (Context.empty |> addExample passing_test |> run_suite
                 |> is_success)
                  be_true);
          };
          {
            name = "Run should return success=false when test case raises";
            f =
              (fun _ ->
                expect
                  (Context.empty |> addExample failing_test |> run_suite
                 |> is_success)
                  be_false);
          };
        ];
  }
  |> run_main
