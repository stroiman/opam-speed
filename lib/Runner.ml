type suite_result = {
  success: bool;
  print_break: bool;
  no_of_failing_examples: int;
  no_of_passing_examples: int;
}

module ExampleRunner = struct
  type test_outcome =
    | Success
    | Failure
    | FailureWithFormat of (Format.formatter -> unit)

  module type EXAMPLE_RUNNER = sig
    type test_function
    type 'b cont_result
    type ('a, 'b) cont = 'a -> test_outcome -> 'b cont_result

    val return : 'b -> 'b cont_result
    val run : test_function -> 'a -> ('a, 'b) cont -> 'b cont_result
    val wait : 'b cont_result -> 'b
  end

  module SyncRunner = struct
    type test_function = Domain.Sync.test_function
    type ('a, 'b) cont = 'a -> test_outcome -> 'b
    type 'b cont_result = 'b

    let wait x = x
    let return = Fun.id

    let run f ctx cont =
      try
        f ();
        cont ctx Success
      with e ->
        ( match e with
          | Assertions.FormattedAssertionError pp ->
            cont ctx (FailureWithFormat pp)
          | exn ->
            cont ctx
              (FailureWithFormat
                 (Format.dprintf "@{<orange>%s@}" (Printexc.to_string exn))
              )
        )
    ;;
  end

  module LwtRunner = struct
    type test_function = unit -> unit Lwt.t
    type ('a, 'b) cont = 'a -> test_outcome -> 'b Lwt.t
    type 'b cont_result = 'b Lwt.t

    let wait x = Lwt_main.run x
    let return = Lwt.return

    let run (f : test_function) (ctx : 'a) (cont : ('a, 'b) cont) : 'b Lwt.t =
      try%lwt
        let%lwt _ = f () in
        cont ctx Success
      with e ->
        ( match e with
          | Assertions.FormattedAssertionError pp ->
            cont ctx (FailureWithFormat pp)
          | exn ->
            cont ctx
              (FailureWithFormat (Format.dprintf "%s" (Printexc.to_string exn)))
        )
    ;;
  end
end

open ExampleRunner

module Reporter = struct
  type t = suite_result

  let empty_suite_result =
    {
      success= true;
      print_break= false;
      no_of_failing_examples= 0;
      no_of_passing_examples= 0;
    }
  ;;

  let is_success { success; _ } = success
end

open Reporter

module Make
    (D : Dsl.DOMAIN)
    (Runner : ExampleRunner.EXAMPLE_RUNNER
              with type test_function = D.test_function) =
struct
  open D

  let rec filter_suite suite =
    let is_not_empty suite =
      suite.examples |> List.length > 0 || suite.child_groups |> List.length > 0
    in
    let examples = suite.examples |> List.filter (fun x -> x.focus) in
    let child_groups =
      suite.child_groups |> List.map filter_suite |> List.filter is_not_empty
    in
    { suite with examples; child_groups }
  ;;

  let start_group name fmt ctx run cont =
    let print_break =
      match name with
      | None -> ctx.print_break
      | Some n ->
        if ctx.print_break then Format.pp_print_cut fmt ();
        Format.fprintf fmt "@[<v2>@{<bold>•@} %s" n;
        true
    in
    run { ctx with print_break } (fun ctx ->
      if Option.is_some name then Format.fprintf fmt "@]";
      ctx |> cont
    )
  ;;

  let start_example name fmt ctx run cont =
    if ctx.print_break then Format.fprintf fmt "@,";
    let cont ctx result =
      Format.pp_open_vbox fmt 2;
      let outcome =
        (* Printf.printf "\nFormat output %s" name; *)
        match result with
        | Success ->
          Format.fprintf fmt "@{<green>✔@} %s" name;
          {
            ctx with
            no_of_passing_examples= ctx.no_of_passing_examples + 1;
            print_break= true;
          }
        | Failure ->
          Format.fprintf fmt "@{<red>✘@} %s" name;
          {
            ctx with
            success= false;
            print_break= true;
            no_of_failing_examples= ctx.no_of_failing_examples + 1;
          }
        | FailureWithFormat pp ->
          Format.fprintf fmt "@{<red>✘@} %s" name;
          Format.fprintf fmt "@,%t" pp;
          {
            ctx with
            success= false;
            print_break= true;
            no_of_failing_examples= ctx.no_of_failing_examples + 1;
          }
      in
      Format.pp_close_box fmt ();
      cont outcome
    in
    run ctx cont
  ;;

  let run_ex fmt ctx (example : D.example) =
    let run ctx cont = Runner.run example.f ctx cont in

    start_example example.name fmt ctx run
  ;;

  let rec run_child_suite fmt ctx suite cont =
    let run_examples ctx cont =
      let rec iter examples ctx cont =
        match examples with
        | [] -> cont ctx
        | x :: xs -> run_ex fmt ctx x (fun ctx -> iter xs ctx cont)
      in
      iter (List.rev suite.examples) ctx cont
    in

    start_group suite.name fmt ctx
      (fun ctx cont ->
        let cont ctx = run_examples ctx cont in
        let rec iter groups ctx cont =
          match groups with
          | [] -> cont ctx
          | x :: xs -> run_child_suite fmt ctx x (fun ctx -> iter xs ctx cont)
        in
        iter (List.rev suite.child_groups) ctx cont
      )
      cont
  ;;

  let run_suite ?(fmt = Ocolor_format.raw_std_formatter) ?(filter = false)
    ?(ctx = empty_suite_result) suite cont
    =
    Format.fprintf fmt "@[<v>";
    let filter = filter || suite.has_focused in
    let suite = if filter then filter_suite suite else suite in
    run_child_suite fmt ctx suite (fun result ->
      Format.pp_close_box fmt ();
      Format.pp_print_flush fmt ();
      cont result
    )
  ;;

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
  ;;

  (* let run_main suite = *)
  (*   let fmt = Ocolor_format.raw_std_formatter in *)
  (*   run_suite ~fmt suite (consume_test_result fmt) *)
  (* ;; *)
  (** This runs the test suite and exits the program. If the test suite is
      successful, it will exit with exit code zero, otherwise it will exit with
      exit code 1. *)
  let has_focused suite = suite.has_focused
end

module SyncRunner = Make (Domain.Sync) (ExampleRunner.SyncRunner)
module LwtRunner = Make (Domain.LwtDomain) (ExampleRunner.LwtRunner)
include SyncRunner

let run_suite = run_suite_wait

let run_suites ~fmt sync_suite lwt_suite =
  let has_focused =
    SyncRunner.has_focused sync_suite || LwtRunner.has_focused lwt_suite
  in
  Lwt_main.run
    (SyncRunner.run_suite ~fmt ~filter:has_focused sync_suite (fun ctx ->
       LwtRunner.run_suite ~fmt ~ctx ~filter:has_focused lwt_suite Lwt.return
     )
    )
;;

(** This runs the test suite and exits the program. If the test suite is
    successful, it will exit with exit code zero, otherwise it will exit with
    exit code 1. *)
let run_root_suites () =
  let fmt = Ocolor_format.raw_std_formatter in
  let result = run_suites ~fmt !Dsl.Sync.root_suite !Dsl.LwtDsl.root_suite in
  consume_test_result fmt result
;;
