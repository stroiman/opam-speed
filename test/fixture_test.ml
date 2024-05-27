open Speed
open Assertions
open Speed.Dsl.Effect.Simple
open Utils

type Speed.metadata += IntValue of int

let get_value = function
  | IntValue x -> Some x
  | _ -> None
;;

root_context "Fixture" (fun _ ->
  test "Initialises the fixture" (fun _ ->
    let actual = ref 0 in
    let suite =
      Domain.(
        make_suite ()
        |> add_fixture
             ~setup:(fun _ -> 42)
             (add_example "test" (fun { subject; _ } -> actual := subject))
      )
    in
    let _result = run_suite_silent suite in
    !actual |> should @@ equal_int 42
  );

  test "It can read metadata from example" (fun _ ->
    let actual = ref 0 in
    Domain.(
      make_suite ()
      |> add_fixture
           ~setup:(fun { metadata; _ } ->
             Base.List.find_map ~f:get_value metadata
             |> Base.Option.value_or_thunk ~default:(fun _ -> 42)
           )
           (fun suite ->
             suite
             |> add_example ~metadata:[IntValue 123] "test"
                  (fun { subject; _ } -> actual := subject
                )
           )
    )
    |> run_suite_silent
    |> ignore;
    !actual |> should @@ equal_int 123;
    ()
  );

  test "It can read metadata from example using ppx" (fun _ ->
    (* This test does exactly the same as above, but uses a ppx transformation
       to simplify looking up metadata
    *)
    let actual = ref 0 in
    Domain.(
      make_suite ()
      |> add_fixture
           ~setup:(fun { metadata; _ } ->
             metadata
             |> [%m IntValue]
             |> Base.Option.value_or_thunk ~default:[%f_ 42]
           )
           (add_example ~metadata:[IntValue 123] "test" (fun { subject; _ } ->
              actual := subject
            )
           )
    )
    |> run_suite_silent
    |> ignore;
    !actual |> should @@ equal_int 123;
    ()
  );

  test "It can read metadata from example using ppx and default value" (fun _ ->
    (* This test does exactly the same as above, but uses a ppx transformation
       to simplify looking up metadata
    *)
    let actual = ref 0 in
    Domain.(
      make_suite ()
      |> add_fixture
           ~setup:(fun { metadata; _ } -> metadata |> [%m IntValue 42])
           (add_example ~metadata:[IntValue 123] "test" (fun { subject; _ } ->
              actual := subject
            )
           )
    )
    |> run_suite_silent
    |> ignore;
    !actual |> should @@ equal_int 123;
    ()
  );

  test "It can read metadata from test input using ppx and default value"
    (fun _ ->
       let actual = ref 0 in
       Domain.(
         make_suite ()
         |> add_fixture ~setup:[%mx IntValue 42]
              (add_example ~metadata:[IntValue 123] "test"
                 (fun { subject; _ } -> actual := subject
               )
              )
       )
       |> run_suite_silent
       |> ignore;
       !actual |> should @@ equal_int 123;
       ()
  );

  test "It can read metadata from parent group" (fun _ ->
    let actual = ref 0 in
    Domain.(
      make_suite ()
      |> add_fixture
           ~setup:(fun { metadata; _ } ->
             Base.List.find_map ~f:get_value metadata
             |> Base.Option.value_or_thunk ~default:[%f_ 42]
           )
           (fun suite ->
             suite
             |> Domain.add_context ~metadata:[IntValue 123] "Parent"
                  (add_example "test" (fun { subject; _ } -> actual := subject))
           )
    )
    |> run_suite_silent
    |> ignore;
    !actual |> should @@ equal_int 123
  );

  test "It uses metadata from example when there's a conflict" (fun _ ->
    let actual = ref 0 in
    Domain.(
      make_suite ()
      |> add_fixture
           ~setup:(fun { metadata; _ } ->
             Base.List.find_map ~f:get_value metadata
             |> Base.Option.value_or_thunk ~default:[%f_ 42]
           )
           (fun suite ->
             suite
             |> Domain.add_context ~metadata:[IntValue 100] "Parent"
                  (add_example ~metadata:[IntValue 123] "test"
                     (fun { subject; _ } -> actual := subject
                   )
                  )
           )
    )
    |> run_suite_silent
    |> ignore;
    !actual |> should @@ equal_int 123
  );

  test "It uses value from innermost group when there's a conflict" (fun _ ->
    let actual = ref 0 in
    Domain.(
      make_suite ()
      |> add_fixture
           ~setup:(fun { metadata; _ } ->
             Base.List.find_map ~f:get_value metadata
             |> Base.Option.value_or_thunk ~default:[%f_ 42]
           )
           (fun suite ->
             suite
             |> Domain.add_context ~metadata:[IntValue 100] "Parent"
                  (Domain.add_context ~metadata:[IntValue 123] "Parent"
                     (add_example "test" (fun { subject; _ } ->
                        actual := subject
                      )
                     )
                  )
           )
    )
    |> run_suite_silent
    |> ignore;
    !actual |> should @@ equal_int 123;
    ()
  );

  test "It works with the DSL" (fun _ ->
    let actual = ref 0 in
    let suite =
      Speed.Dsl.Effect.(
        parse (fun s ->
          s.context "root" (fun s ->
            s.fixture "child" ~setup:[%mx IntValue 42] (fun s ->
              with_metadata [IntValue 123] s.test "Test" (fun { subject; _ } ->
                actual := subject
              )
            )
          )
        )
      )
    in
    suite |> run_suite_silent |> ignore;
    !actual |> should @@ equal_int 123;
    ()
  );

  test "It works with out a name" (fun _ ->
    let actual = ref 0 in
    let suite =
      Speed.Dsl.Effect.(
        parse (fun s ->
          s.context "root" (fun s ->
            s.setup [%mx IntValue 42] (fun s ->
              s.test ~metadata:[IntValue 123] "Test" (fun { subject; _ } ->
                actual := subject
              )
            )
          )
        )
      )
    in
    suite |> run_suite_silent |> ignore;
    !actual |> should @@ equal_int 123;
    ()
  )
)
