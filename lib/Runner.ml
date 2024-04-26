open Domain

type test_result = { success : bool }

let run suite =
  try
    suite.examples |> List.iter (fun x -> x.f ());

    { success = true }
  with _ -> { success = false }

let is_success { success } = success
