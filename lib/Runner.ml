open Domain

module Reporter = struct
  type end_test = unit

  type test_result = {
    success: bool;
    print_break: bool;
    no_of_failing_examples: int;
    no_of_passing_examples: int;
  }

  type t = test_result

  type test_outcome =
    | Success
    | Failure
    | FailureWithFormat of (Format.formatter -> unit)

  let empty =
    {
      success= true;
      print_break= false;
      no_of_failing_examples= 0;
      no_of_passing_examples= 0;
    }
  ;;

  let is_success { success; _ } = success

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

  let start_example name fmt ctx run =
    if ctx.print_break then Format.fprintf fmt "@,";
    run ctx (fun ctx result cont ->
      let outcome =
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
      cont outcome
    )
  ;;
end

open Reporter

let run_ex fmt ctx (example : Domain.example) =
  start_example example.name fmt ctx (fun ctx cont ->
    try
      example.f ();
      cont ctx Success
    with e ->
      ( match e with
        | Assertions.FormattedAssertionError pp ->
          cont ctx (FailureWithFormat pp)
        | _ -> cont ctx Failure
      )
  )
;;

let run_e fmt ctx (example : Domain.example) : test_result =
  run_ex fmt ctx example (fun r -> r)
;;

let rec run_child_suite fmt ctx suite outer_cont =
  let run_examples ctx cont =
    List.fold_left (run_e fmt) ctx (List.rev suite.examples) |> cont
  in

  start_group suite.name fmt ctx
    (fun ctx cont ->
      match suite.child_groups with
      | [] -> run_examples ctx cont
      | grps ->
        List.fold_left
          (fun cont grp ctx -> run_child_suite fmt ctx grp cont)
          (fun ctx -> run_examples ctx cont)
          grps ctx
    )
    outer_cont
;;

let run_suite ?(fmt = Ocolor_format.raw_std_formatter) suite =
  Format.fprintf fmt "@[<v>";
  let ctx = empty in
  let result = run_child_suite fmt ctx suite Fun.id in
  Format.pp_close_box fmt ();
  Format.pp_print_flush fmt ();
  result
;;

let is_success { success; _ } = success
let get_no_of_failing_examples x = x.no_of_failing_examples
let get_no_of_passing_examples x = x.no_of_passing_examples

(** This runs the test suite and exits the program. If the test suite is
    successful, it will exit with exit code zero, otherwise it will exit with
    exit code 1. *)
let run_main suite =
  let fmt = Ocolor_format.raw_std_formatter in
  let result = run_suite suite in
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
