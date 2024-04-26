open Speed.Domain
open Speed.Runner

exception TestError

let passing_test () = ()
let failing_test () = raise TestError

let () =
  let expected_success =
    { examples = [ { name = "Passing example"; f = passing_test } ] }
    |> run |> is_success
  in
  Printf.printf "First run: %b\n" expected_success;
  let expected_failure =
    { examples = [ { name = "Failing example"; f = failing_test } ] }
    |> run |> is_success
  in
  Printf.printf "Second run: %b\n" expected_failure
