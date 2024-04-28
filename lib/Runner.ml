open Domain

type test_result = {
  success: bool;
  no_of_failing_examples: int;
  no_of_passing_examples: int;
}

let empty = { success= true; no_of_failing_examples= 0; no_of_passing_examples= 0 }

let run_example ?(print_break = true) fmt ctx example =
  if print_break then Format.fprintf fmt "@,";
  Format.fprintf fmt "@[<v2>";
  let result =
    try
      (* Format.pp_print_flush fmt (); *)
      example.f ();
      Format.fprintf fmt "@{<green>✔@} %s" example.name;
      { ctx with no_of_passing_examples= ctx.no_of_passing_examples + 1 }
    with
    | e ->
      Format.fprintf fmt "@{<red>✘@} %s" example.name;
      (match e with
       | Assertions.FormattedAssertionError pp -> Format.fprintf fmt "@,%t" pp
       | _ -> ());
      { ctx with success= false; no_of_failing_examples= ctx.no_of_failing_examples + 1 }
  in
  Format.fprintf fmt "@]";
  result
;;

let id x = x

let rec run_child_suite ?(print_break = false) fmt ctx suite =
  let print_break =
    match suite.name with
    | None -> print_break
    | Some n ->
      if print_break then Format.pp_print_cut fmt ();
      Format.fprintf fmt "@[<v2>@{<bold>•@} %s" n;
      true
  in
  let ctx =
    match List.rev suite.child_groups with
    | [] -> ctx
    | hd :: tl ->
      let ctx = run_child_suite ~print_break fmt ctx hd in
      tl |> List.fold_left (run_child_suite ~print_break:true fmt) ctx
  in
  let result =
    match List.rev suite.examples with
    | [] -> ctx
    | hd :: tl ->
      let ctx = run_example ~print_break fmt ctx hd in
      tl |> List.fold_left (run_example ~print_break:true fmt) ctx
  in
  if Option.is_some suite.name then Format.fprintf fmt "@]";
  result
;;

let run_suite ?(fmt = Ocolor_format.raw_std_formatter) suite =
  Format.fprintf fmt "@[<v>";
  let ctx = empty in
  let result = run_child_suite fmt ctx suite in
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
      0)
    else (
      Format.fprintf fmt "@{<red>FAIL@}";
      1)
  in
  Format.fprintf fmt "@,Passing tests: %d@,Failing tests: %d@,@]" passing failing;
  Format.pp_print_flush fmt ();
  exit exit_code
;;
