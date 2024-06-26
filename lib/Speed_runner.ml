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

let get_no_of_executed_examples x =
  x.no_of_passing_examples + x.no_of_failing_examples

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

let split_result r =
  ( { (make_result ?fmt:r.fmt ()) with print_break= r.print_break },
    { r with fmt= None } )

let join_result r1 r2 =
  let fmt =
    match r1.fmt, r2.fmt with
    | Some _, Some _ -> failwith "Two fmts"
    | None, Some x -> Some x
    | Some x, None -> Some x
    | None, None -> None
  in
  let tmp =
    {
      fmt;
      print_break= r1.print_break || r2.print_break;
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

let add_pp ?(no_break = false) pp ctx =
  join_result
    { ctx with print_break= ctx.print_break && not no_break }
    { (make_result ()) with pp= Some pp }

module ExampleRunner = struct
  type test_outcome =
    | Success
    | FailureWithFormat of (Format.formatter -> unit)

  module type EXAMPLE_RUNNER = sig
    type 'a test_function
    type 'a cont_result

    val return : 'a -> 'a cont_result

    val run
      :  'a test_function ->
      'a Domain.test_input ->
      test_outcome cont_result

    val wait : 'a cont_result -> 'a
    val bind : ('a -> 'b cont_result) -> 'a cont_result -> 'b cont_result
    val map : ('a -> 'b) -> 'a cont_result -> 'b cont_result
  end

  module SyncRunner = struct
    type 'a test_function = 'a Domain.Sync.test_function
    type 'a cont_result = 'a
    type 'a cont = test_outcome -> 'a cont_result

    let wait x = x
    let return = Fun.id
    let bind f x = f x
    let map f x = f x

    let run f input =
      try
        f input;
        Success
      with e ->
        ( match e with
          | Assertions.FormattedAssertionError pp -> FailureWithFormat pp
          | exn ->
            FailureWithFormat
              (Format.dprintf "@{<orange>%s@}" (Printexc.to_string exn))
        )
  end

  module LwtRunner = struct
    type 'a test_function = 'a Domain.LwtDomain.test_function
    type 'a cont = test_outcome -> 'a Lwt.t
    type 'a cont_result = 'a Lwt.t

    let wait = Lwt_main.run
    let return = Lwt.return
    let bind f x = Lwt.bind x f
    let map = Lwt.map

    let run f input =
      try%lwt
        let%lwt _ = f input in
        Lwt.return Success
      with e ->
        ( match e with
          | Assertions.FormattedAssertionError pp ->
            Lwt.return (FailureWithFormat pp)
          | exn ->
            Lwt.return
              (FailureWithFormat (Format.dprintf "%s" (Printexc.to_string exn)))
        )
  end
end

open ExampleRunner

module Reporter = struct
  type t = suite_result

  let add_test_outcom ctx name result =
    match result with
    | Success ->
      { ctx with no_of_passing_examples= ctx.no_of_passing_examples + 1 }
      |> add_pp (Format.dprintf "@{<green>✔@} %s" name)
    | FailureWithFormat pp ->
      {
        ctx with
        success= false;
        no_of_failing_examples= ctx.no_of_failing_examples + 1;
      }
      |> add_pp (Format.dprintf "@{<red>✘@} %s@,%t" name pp)

  let is_success { success; _ } = success

  let start_group name ctx =
    let ctx, cont_ctx = split_result ctx in
    let ctx = ctx |> add_pp (Format.dprintf "@[<v2>@{<bold>•@} %s" name) in
    let end_group ctx =
      let ctx = join_result ctx cont_ctx in
      ctx |> add_pp ~no_break:true (Format.dprintf "@]")
    in
    ctx, end_group

  let start_example name ctx =
    let ex_ctx, cont_ctx = split_result ctx in
    let end_example result ctx =
      let ex_result = add_test_outcom ex_ctx name result in
      join_result ex_result ctx
    in
    cont_ctx, end_example
end

module Make
    (D : Domain.DOMAIN)
    (Runner : ExampleRunner.EXAMPLE_RUNNER
              with type 'a test_function = 'a D.test_function) =
struct
  open D

  let ( >>= ) x f = Runner.bind f x
  let ( >|= ) x f = Runner.map f x

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

  let rec run_setup : 'b. metadata list -> (unit, 'b) setup_stack -> 'b =
    fun metadata -> function
    | Root f -> f Domain.TestInput.{ metadata; subject= () }
    | Stack (f, g) -> g { metadata; subject= run_setup metadata f }

  let run_ex metadata setups ctx (example : 'a D.example) =
    let ctx, end_example = Reporter.start_example example.name ctx in
    let metadata = example.metadata @ metadata in
    let test_input = run_setup metadata setups in
    let outcome = Runner.run example.f { subject= test_input; metadata } in
    let cont ctx = outcome >|= fun o -> end_example o ctx in
    ctx, cont

  let rec run_child_suite
    : type a.
      a D.t ->
      'result ->
      metadata list ->
      (unit, a) setup_stack ->
      'b continuation
    =
    fun suite ctx metadata setups cont ->
    let metadata = suite.metadata @ metadata in
    let run_group ctx cont =
      let run_examples cont ctx =
        let cont_ctx, test_outcomes =
          suite.examples
          |> List.rev
          |> List.fold_left_map (run_ex metadata setups) ctx
        in
        List.fold_left
          (fun acc result -> acc >>= result)
          (Runner.return cont_ctx) test_outcomes
        >>= cont
      in
      let run_child_groups_and_then run_examples =
        let rec iter groups ctx =
          let run_child child cont =
            match child with
            | Child { child; setup } ->
              let setups = Stack (setups, setup) in
              run_child_suite child ctx metadata setups cont
            | Context { child } ->
              run_child_suite child ctx metadata setups cont
          in
          match groups with
          | [] -> run_examples ctx
          | child :: xs -> run_child child (iter xs)
        in
        iter (List.rev suite.child_groups) ctx
      in
      run_child_groups_and_then (run_examples cont)
    in
    match suite.name with
    | Some name ->
      let ctx, end_group = Reporter.start_group name ctx in
      run_group ctx (fun ctx -> ctx |> end_group |> cont)
    | None -> run_group ctx cont

  let run_suite ?(fmt = Ocolor_format.raw_std_formatter) ?(filter = false) ~ctx
    s cont
    =
    match s with
    | suite ->
      Format.fprintf fmt "@[<v>";
      let filter = filter || suite.has_focused in
      let suite = if filter then filter_suite s else s in
      let x =
        run_child_suite suite ctx []
          (Root (fun _ -> ()))
          (fun result ->
            Format.pp_close_box fmt ();
            Format.pp_print_flush fmt ();
            cont result
          )
      in
      x

  let wait = Runner.wait

  let run_suite_return ?fmt suite =
    run_suite ?fmt ~ctx:(make_result ?fmt ()) suite Runner.return

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

let run_suites ?(fmt = Ocolor_format.raw_std_formatter) sync_suite lwt_suite =
  let has_focused =
    SyncRunner.has_focused sync_suite || LwtRunner.has_focused lwt_suite
  in
  let ctx = make_result ~fmt () in
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
