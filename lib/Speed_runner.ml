module Domain = Speed_domain
module Dsl = Speed_dsl_list
module Assertions = Speed_assertions

type metadata = Speed_metadata.t

type suite_result = {
  fmt: Format.formatter option;
  print_break: bool;
  success: bool;
  no_of_failing_examples: int;
  no_of_passing_examples: int;
  pp: (Format.formatter -> unit) option;
}

let ( >> ) f g x = g (f x)

type 'a continuation = (suite_result -> 'a) -> 'a

let fst (x, _) = x

let empty_suite_result =
  {
    fmt= None;
    print_break= false;
    success= true;
    no_of_failing_examples= 0;
    no_of_passing_examples= 0;
    pp= None;
  }

let make_result ?fmt () = { empty_suite_result with fmt }

let join_result r1 r2 =
  let fmt = r1.fmt in
  let tmp =
    {
      fmt;
      print_break= r1.print_break;
      success= r1.success && r2.success;
      no_of_failing_examples=
        r1.no_of_failing_examples + r2.no_of_failing_examples;
      no_of_passing_examples=
        r1.no_of_passing_examples + r2.no_of_passing_examples;
      pp=
        ( match r1.pp, r2.pp with
          | Some x, Some y ->
            Some
              (fun fmt ->
                x fmt;
                Format.pp_print_cut fmt ();
                y fmt
              )
          | Some x, None -> Some x
          | None, Some y -> Some y
          | _ -> None
        );
    }
  in
  match tmp.fmt, tmp.pp with
  | Some fmt, Some pp ->
    if tmp.print_break then Format.fprintf fmt "@,";
    pp fmt;
    { tmp with pp= None; print_break= true }
  | _ -> tmp

module ExampleRunner = struct
  type test_outcome =
    | Success
    | Failure
    | FailureWithFormat of (Format.formatter -> unit)

  module type EXAMPLE_RUNNER = sig
    type test_function
    type cont_result
    type cont = test_outcome -> cont_result

    val return : suite_result -> cont_result

    val to_callback
      :  cont_result ->
      (suite_result -> cont_result) ->
      cont_result

    val run : test_function -> cont -> cont_result
    val wait : cont_result -> suite_result
    val join : cont_result -> cont_result -> cont_result
    val bind : (suite_result -> cont_result) -> cont_result -> cont_result
    val map : (suite_result -> suite_result) -> cont_result -> cont_result
  end

  module SyncRunner = struct
    type test_function = unit Domain.Sync.test_function
    type cont_result = suite_result
    type cont = test_outcome -> cont_result

    let wait x = x
    let return = Fun.id
    let join = join_result
    let bind f x = f x
    let to_callback x f = f x
    let map f x = f x

    let run (f : test_function) cont =
      try
        f { metadata= []; subject= () };
        cont Success
      with e ->
        ( match e with
          | Assertions.FormattedAssertionError pp -> cont (FailureWithFormat pp)
          | exn ->
            cont
              (FailureWithFormat
                 (Format.dprintf "@{<orange>%s@}" (Printexc.to_string exn))
              )
        )
  end

  module LwtRunner = struct
    type test_function = unit Domain.test_input -> unit Lwt.t
    type cont = test_outcome -> suite_result Lwt.t
    type cont_result = suite_result Lwt.t

    let wait x = Lwt_main.run x
    let return = Lwt.return
    let bind f x = Lwt.bind x f

    let join a b =
      let%lwt r1 = a in
      let%lwt r2 = b in
      Lwt.return @@ join_result r1 r2

    let map f x = x |> Lwt.map f

    let to_callback x f =
      let%lwt v = x in
      f v

    let run (f : test_function) cont =
      try%lwt
        let%lwt _ = f { metadata= []; subject= () } in
        cont Success
      with e ->
        ( match e with
          | Assertions.FormattedAssertionError pp -> cont (FailureWithFormat pp)
          | exn ->
            cont
              (FailureWithFormat (Format.dprintf "%s" (Printexc.to_string exn)))
        )
  end
end

open ExampleRunner

module Reporter = struct
  type t = suite_result

  let is_success { success; _ } = success
end

module Make
    (D : Domain.DOMAIN)
    (Runner : ExampleRunner.EXAMPLE_RUNNER
              with type test_function = unit D.test_function) =
struct
  open D

  type ('a, 'b) setup_stack =
    | Root : (unit Domain.test_input -> 'a) -> (unit, 'a) setup_stack
    | Stack :
        ('a, 'b) setup_stack * ('b Domain.test_input -> 'c)
        -> ('a, 'c) setup_stack

  let rec filter_suite : 'a. 'a D.t -> 'a D.t = function
    | suite ->
      if suite.focus
      then suite
      else (
        let is_not_empty = function
          | Child { child= suite; _ } ->
            suite.examples |> List.length > 0
            || suite.child_groups |> List.length > 0
          | Context { child= suite } ->
            suite.examples |> List.length > 0
            || suite.child_groups |> List.length > 0
        in
        let examples =
          suite.examples |> List.filter (fun (x : 'a D.example) -> x.focus)
        in
        let child_groups =
          suite.child_groups
          |> List.map filter_suite_mixed
          |> List.filter is_not_empty
        in
        { suite with examples; child_groups }
      )

  and filter_suite_mixed : 'a. 'a child_suite -> 'a child_suite = function
    | Child { child; setup } -> Child { setup; child= filter_suite child }
    | Context { child } -> Context { child= filter_suite child }

  let start_group name print_break_after fmt run cont =
    ( match name with
      | None -> ()
      | Some n -> Format.fprintf fmt "@[<v2>@{<bold>•@} %s@," n
    );
    run (fun ctx ->
      if Option.is_some name
      then (
        Format.fprintf fmt "@]";
        if print_break_after then Format.fprintf fmt "@,"
      );
      ctx |> cont
    )

  let start_example name run cont =
    let continue_from_example_result result =
      let outcome =
        match result with
        | Success ->
          {
            empty_suite_result with
            no_of_passing_examples=
              empty_suite_result.no_of_passing_examples + 1;
            pp= Some (Format.dprintf "@{<green>✔@} %s" name);
          }
        | Failure ->
          {
            empty_suite_result with
            success= false;
            no_of_failing_examples=
              empty_suite_result.no_of_failing_examples + 1;
            pp= Some (Format.dprintf "@{<red>✘@} %s" name);
          }
        | FailureWithFormat pp ->
          {
            empty_suite_result with
            success= false;
            no_of_failing_examples=
              empty_suite_result.no_of_failing_examples + 1;
            pp=
              Some
                (fun fmt ->
                  Format.fprintf fmt "@{<red>✘@} %s" name;
                  Format.fprintf fmt "@,%t" pp
                );
          }
      in
      cont outcome
    in
    run continue_from_example_result

  let rec run_setup : 'b. metadata list -> (unit, 'b) setup_stack -> 'b =
    fun metadata -> function
    | Root f -> f Domain.TestInput.{ metadata; subject= () }
    | Stack (f, g) -> g { metadata; subject= run_setup metadata f }

  let run_ex (example : 'a D.example) metadata setups cont =
    let test_input = run_setup (example.metadata @ metadata) setups in
    let run cont =
      Runner.run (fun d -> example.f { d with subject= test_input }) cont
    in

    start_example example.name run cont

  let rec run_child_suite
    : type a.
      Format.formatter ->
      bool ->
      a D.t ->
      metadata list ->
      (unit, a) setup_stack ->
      'b continuation
    =
    fun fmt print_break_after suite metadata setups cont ->
    let metadata = suite.metadata @ metadata in
    let run_group cont =
      let run_examples ctx =
        suite.examples
        |> List.rev
        |> List.map (fun ex -> run_ex ex metadata setups Runner.return)
        |> List.fold_left (fun acc a -> Runner.join acc a) (Runner.return ctx)
        |> Runner.bind cont
      in
      let run_child_groups run_examples =
        let _print_break_after = List.length suite.examples > 0 in
        let rec iter groups ctx =
          let run_child print_break_after child setups cont =
            let cont c = cont @@ join_result ctx c in
            match child with
            | Child { child; setup } ->
              let setups = Stack (setups, setup) in
              run_child_suite fmt print_break_after child metadata setups cont
            | Context { child } ->
              run_child_suite fmt print_break_after child metadata setups cont
          in
          match groups with
          | [] -> run_examples ctx
          | child :: [] ->
            run_child _print_break_after child setups run_examples
          | child :: xs -> run_child true child setups (fun ctx -> iter xs ctx)
        in
        let empty_result = make_result ~fmt () in
        iter (List.rev suite.child_groups) empty_result
      in
      run_child_groups (fun ctx -> run_examples ctx)
    in
    start_group suite.name print_break_after fmt run_group cont

  let run_suite ?(fmt = Ocolor_format.raw_std_formatter) ?(filter = false)
    ?(ctx = empty_suite_result) s cont
    =
    match s with
    | suite ->
      Format.fprintf fmt "@[<v>";
      let filter = filter || suite.has_focused in
      let suite = if filter then filter_suite s else s in
      let x =
        run_child_suite fmt false suite []
          (Root (fun _ -> ()))
          (fun result ->
            Format.pp_close_box fmt ();
            Format.pp_print_flush fmt ();
            cont (join_result ctx result)
          )
      in
      x

  let wait = Runner.wait
  let run_suite_return ?fmt suite = run_suite ?fmt suite Runner.return
  let run_suite_wait ?fmt suite = run_suite_return ?fmt suite |> wait
  let is_success { success; _ } = success
  let get_no_of_failing_examples x = x.no_of_failing_examples
  let get_no_of_passing_examples x = x.no_of_passing_examples

  let consume_test_result fmt result =
    let failing = get_no_of_failing_examples result in
    let passing = get_no_of_passing_examples result in
    Format.fprintf fmt "\n@,@[<v2>SUMMARY: ";
    let exit_code =
      if is_success result
      then (
        Format.fprintf fmt "@{<green>PASS@}";
        0
      )
      else (
        Format.fprintf fmt "@{<red>FAIL@}";
        1
      )
    in
    Format.fprintf fmt "@,Passing tests: %d@,Failing tests: %d@,@]" passing
      failing;
    Format.pp_print_flush fmt ();
    exit exit_code

  (** This runs the test suite and exits the program. If the test suite is
      successful, it will exit with exit code zero, otherwise it will exit with
      exit code 1. *)
  let has_focused = function
    | suite -> suite.has_focused
end

module SyncRunner = Make (Domain.Sync) (ExampleRunner.SyncRunner)
module LwtRunner = Make (Domain.LwtDomain) (ExampleRunner.LwtRunner)
include SyncRunner

let run_suite = run_suite_wait

let run_suites ~fmt sync_suite lwt_suite =
  let has_focused =
    SyncRunner.has_focused sync_suite || LwtRunner.has_focused lwt_suite
  in
  let ctx =
    { empty_suite_result with fmt= Some Ocolor_format.raw_std_formatter }
  in
  Lwt_main.run
    (let ctx =
       SyncRunner.run_suite ~fmt ~ctx ~filter:has_focused sync_suite Fun.id
     in
     LwtRunner.run_suite ~fmt ~ctx ~filter:has_focused lwt_suite Lwt.return
    )

(** This runs the test suite and exits the program. If the test suite is
    successful, it will exit with exit code zero, otherwise it will exit with
    exit code 1. *)
let run_root_suites () =
  let fmt = Ocolor_format.raw_std_formatter in
  let result = run_suites ~fmt !Dsl.Sync.root_suite !Dsl.LwtDsl.root_suite in
  consume_test_result fmt result
