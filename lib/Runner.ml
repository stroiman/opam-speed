open Domain

type test_result = {
  success: bool;
  no_of_failing_examples: int;
  no_of_passing_examples: int;
}

let empty = { success= true; no_of_failing_examples= 0; no_of_passing_examples= 0 }

let unwind ?on_error ?on_success ~(protect : 'a -> unit) f x =
  try
    let y = f x in
    (match on_success with
     | Some x -> x ()
     | None -> ());
    protect x;
    y
  with
  | e ->
    (match on_error with
     | Some x -> x ()
     | None -> ());
    protect x;
    raise e
;;

let run_example ?(print_break = true) fmt ctx example =
  if print_break then Format.fprintf fmt "@,";
  let result =
    try
      Format.pp_print_flush fmt ();
      example.f ();
      Format.fprintf fmt "@[<v2>@{<green>✔@} %s" example.name;
      { ctx with no_of_passing_examples= ctx.no_of_passing_examples + 1 }
    with
    | e ->
      Format.fprintf fmt "@[<v2>@{<red>✘@} %s" example.name;
      (match e with
       | Assertions.FormattedAssertionError pp -> Format.fprintf fmt "@,%t" pp
       | _ -> ());
      { ctx with success= false; no_of_failing_examples= ctx.no_of_failing_examples + 1 }
  in
  Format.fprintf fmt "@]";
  result
;;

let id x = x

let rec run_child_suite fmt ctx suite =
  Option.fold ~none:() ~some:(fun n -> Format.fprintf fmt "@[<v2>- %s@," n) suite.name;
  let result =
    let c2 = suite.child_groups |> List.fold_left (run_child_suite fmt) ctx in
    match suite.examples with
    | [] -> c2
    | hd :: tl ->
      let c3 = run_example ~print_break:false fmt c2 hd in
      tl |> List.fold_left (run_example ~print_break:true fmt) c3
  in
  Option.fold ~none:() ~some:(fun _ -> Format.fprintf fmt "@]") suite.name;
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
  match is_success @@ run_suite suite with
  | true -> exit 0
  | false -> exit 1
;;
