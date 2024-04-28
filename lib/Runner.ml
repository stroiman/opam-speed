open Domain

type test_result = { success: bool }

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

let run_example fmt example =
  Format.fprintf fmt "@[<v2>%s@," example.name;
  unwind
    ~on_error:(fun _ -> Format.pp_print_string fmt "Failure")
    ~on_success:(fun _ -> Format.pp_print_string fmt "Success")
    ~protect:(fun _ -> Format.fprintf fmt "@]@,")
    example.f
    ()
;;

let id x = x

let run_suite ?fmt suite =
  let f = fmt |> Option.fold ~none:(Format.get_std_formatter ()) ~some:id in
  Format.fprintf f "@[<v>";
  let result =
    try
      suite.examples |> List.iter (run_example f);
      { success= true }
    with
    | _ -> { success= false }
  in
  Format.pp_close_box f ();
  result
;;

let is_success { success } = success

(** This runs the test suite and exits the program. If the test suite is
    successful, it will exit with exit code zero, otherwise it will exit with
    exit code 1. *)
let run_main suite =
  match is_success @@ run_suite suite with
  | true -> exit 0
  | false -> exit 1
;;
