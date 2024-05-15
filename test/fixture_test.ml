open Speed
open Runner
open Assertions
open Effect_dsl
open Utils.Null_formatter

type Domain.metadata += IntValue of int

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
             (fun suite ->
               suite
               |> add_example "test" (fun { subject; _ } -> actual := subject)
             )
      )
    in
    let _result = run_suite ~fmt suite in
    !actual |> should @@ equal_int 42
  );

  test "It can read metadata from example" (fun _ ->
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
             |> add_example ~metadata:[IntValue 123] "test"
                  (fun { subject; _ } -> actual := subject
                )
           )
    )
    |> run_suite ~fmt
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
    |> run_suite ~fmt
    |> ignore;
    !actual |> should @@ equal_int 123;
    ()
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
    |> run_suite ~fmt
    |> ignore;
    !actual |> should @@ equal_int 123;
    ()
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
    |> run_suite ~fmt
    |> ignore;
    !actual |> should @@ equal_int 123;
    ()
  )
)
