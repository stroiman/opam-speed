open Speed.Dsl

let () =
  let test_case = test (fun _ -> Printf.printf "Hello, world") in
  test_case ()
