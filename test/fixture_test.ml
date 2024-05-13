open Speed
open Runner
open Assertions
open Effect_dsl
open Utils.Null_formatter;;

root_context "Fixture" (fun _ ->
  test "Initialises the fixture" (fun _ ->
    let actual = ref 0 in

    let suite =
      Domain.(
        make_suite ()
        |> add_fixture
             ~setup:(fun () -> 42)
             (fun suite ->
               suite |> add_example "test" (fun ctx -> actual := ctx)
             )
      )
    in
    let _result = run_suite ~fmt suite in
    !actual |> should @@ equal_int 42
  );

  test "Another, just for layout" (fun _ -> ())
)
