open Domain

let run suite =
  try
    suite.examples |> List.iter (fun x -> x.f ());
    true
  with _ -> false
