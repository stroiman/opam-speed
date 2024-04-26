open Speed.Domain
open Speed.Runner

exception TestError

let passing_test () = ()
let failing_test () = raise TestError

let () =
  let expected_success =
    run { examples = [ { name = "Passing example"; f = passing_test } ] }
  in
  Printf.printf "First run: %b\n" expected_success;
  let expected_failure =
    run { examples = [ { name = "Failing example"; f = failing_test } ] }
  in
  Printf.printf "Second run: %b\n" expected_failure
