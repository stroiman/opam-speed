open Domain

type test_result = { success: bool }

let run_suite suite =
  try
    suite.examples |> List.iter (fun x -> x.f ());
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
