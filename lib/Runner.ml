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

let run_example example =
  Format.open_vbox 2;
  Format.print_string example.name;
  Format.print_cut ();
  unwind
    ~on_error:(fun _ -> Format.print_string "Failure")
    ~on_success:(fun _ -> Format.print_string "Success")
    ~protect:(fun _ ->
      Format.close_box ();
      Format.print_cut ())
    example.f
    ()
;;

let run_suite suite =
  try
    suite.examples |> List.iter run_example;
    { success= true }
  with
  | _ -> { success= false }
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
