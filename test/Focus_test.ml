open Speed
open Assertions
open Dsl.Effect.Simple
open Runner
open Utils
open Null_formatter;;

root_context "Focused tests" (fun _ ->
  test "Should run focused examples"
    [%f_
      let open Dsl in
      let ex1 = ref false in
      let ex2 = ref false in
      let suite =
        parse
          [test ~focus:true "1" [%f_ ex1 := true]; test "2" [%f_ ex2 := true]]
      in
      let _result = run_suite ~fmt suite |> get_no_of_failing_examples in
      !ex1 |> should ~name:"Ex1 run" @@ be_true;
      !ex2 |> should ~name:"Ex2 run" @@ be_false];

  test "Should run focused when grouped in contexts examples"
    [%f_
      let open Dsl in
      let ex1 = ref false in
      let ex2 = ref false in
      let suite =
        parse
          [
            context "ctx1"
              [context "ctx1a" [test ~focus:true "1" [%f_ ex1 := true]]];
            context "ctx2" [context "ctx2a" [test "2" [%f_ ex2 := true]]];
          ]
      in
      let _result = run_suite ~fmt suite |> get_no_of_failing_examples in
      !ex1 |> should ~name:"Ex1 run" @@ be_true;
      !ex2 |> should ~name:"Ex2 run" @@ be_false];

  test "should deal with both sync and async" (fun _ ->
    let ex1 = ref false in
    let ex2 = ref false in
    let sync_suite = Dsl.Sync.(parse [test "1" [%f_ ex1 := true]]) in
    let lwt_suite =
      Speed.Dsl.List.LwtDsl.(
        parse
          [
            test ~focus:true "2"
              [%f_
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
  );

  context "Groups are focused" (fun _ ->
    test "Should run the focused group" (fun _ ->
      let ex1 = ref false in
      let ex2 = ref false in
      let ex3 = ref false in
      let ex4 = ref false in
      let suite =
        Speed.Dsl.List.(
          parse
            [
              context "Group 1" [test "Ex 1" (fun _ -> ex1 := true)];
              context ~focus:true "Group 2"
                [
                  test "Ex 2" (fun _ -> ex2 := true);
                  test "Ex 3" (fun _ -> ex3 := true);
                ];
              context "Group 3" [test "Ex 3" (fun _ -> ex4 := true)];
            ]
        )
      in
      run_suite suite |> ignore;
      !ex1 |> should be_false;
      !ex2 |> should be_true;
      !ex3 |> should be_true;
      !ex4 |> should be_false
    );

    test "Should run all examples inside a focused group" (fun _ ->
      (* This is not explicitly by design. A focused test is returned
         unfiltered. This test just documents that behaviour. It may change *)
      let ex1 = ref false in
      let ex2 = ref false in
      let ex3 = ref false in
      let suite =
        Speed.Dsl.List.(
          parse
            [
              context "Group 1" [test "Ex 1" (fun _ -> ex1 := true)];
              context ~focus:true "Group 2"
                [
                  test "Ex 2" ~focus:true (fun _ -> ex2 := true);
                  test "Ex 3" (fun _ -> ex3 := true);
                ];
            ]
        )
      in
      run_suite ~fmt suite |> ignore;
      !ex1 |> should be_false;
      !ex2 |> should be_true;
      !ex3 |> should be_true
    );

    test "Should run all examples inside a focused group - simple effect DSL"
      (fun _ ->
         (* This is not explicitly by design. A focused test is returned
            unfiltered. This test just documents that behaviour. It may change *)
         let ex1 = ref false in
         let ex2 = ref false in
         let suite =
           parse (fun _ ->
             context "Group 1" (fun _ -> test "Ex 1" (fun _ -> ex1 := true));
             context ~focus:true "Group 2" (fun _ ->
               test "Ex 2" (fun _ -> ex2 := true)
             )
           )
         in
         run_suite ~fmt suite |> ignore;
         !ex1 |> should be_false;
         !ex2 |> should be_true
    );

    test "Should run all examples inside a focused group - effect DSL" (fun _ ->
      (* This is not explicitly by design. A focused test is returned
         unfiltered. This test just documents that behaviour. It may change *)
      let ex1 = ref false in
      let ex2 = ref false in
      let suite =
        Speed.Dsl.Effect.(
          parse (fun s ->
            s.context "Group 1" (fun s -> s.test "Ex 1" (fun _ -> ex1 := true));
            s.context ~focus:true "Group 2" (fun s ->
              s.test "Ex 2" (fun _ -> ex2 := true)
            )
          )
        )
      in
      run_suite ~fmt suite |> ignore;
      !ex1 |> should be_false;
      !ex2 |> should be_true
    )
  )
)
