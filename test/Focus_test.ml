open Speed
open Assertions
open Effect_dsl
open Runner
open Utils
open Null_formatter;;

root_context "Focused tests" (fun _ ->
  test "Should run focused examples"
    [%f
      let open Dsl in
      let ex1 = ref false in
      let ex2 = ref false in
      let suite =
        parse [test ~focus:true "1" [%f ex1 := true]; test "2" [%f ex2 := true]]
      in
      let _result = run_suite ~fmt suite |> get_no_of_failing_examples in
      !ex1 |> should ~name:"Ex1 run" @@ be_true;
      !ex2 |> should ~name:"Ex2 run" @@ be_false];

  test "Should run focused when grouped in contexts examples"
    [%f
      let open Dsl in
      let ex1 = ref false in
      let ex2 = ref false in
      let suite =
        parse
          [
            context "ctx1"
              [context "ctx1a" [test ~focus:true "1" [%f ex1 := true]]];
            context "ctx2" [context "ctx2a" [test "2" [%f ex2 := true]]];
          ]
      in
      let _result = run_suite ~fmt suite |> get_no_of_failing_examples in
      !ex1 |> should ~name:"Ex1 run" @@ be_true;
      !ex2 |> should ~name:"Ex2 run" @@ be_false];

  test "should deal with both sync and async" (fun _ ->
    let ex1 = ref false in
    let ex2 = ref false in
    let sync_suite = Dsl.Sync.(parse [test "1" [%f ex1 := true]]) in
    let lwt_suite =
      Speed.Dsl.LwtDsl.(
        parse
          [
            test ~focus:true "2"
              [%f
                ex2 := true;
                Lwt.return ()];
          ]
      )
    in

    let _result =
      run_suites ~fmt sync_suite lwt_suite |> get_no_of_failing_examples
    in
    !ex1 |> should ~name:"Ex1 run" @@ be_false;
    !ex2 |> should ~name:"Ex2 run" @@ be_true
  )
)
