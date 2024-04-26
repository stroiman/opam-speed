open Speed.Domain
open Speed.Runner
open Speed.Assertions

exception TestError

let passing_test () = ()
let failing_test () = raise TestError

let () =
  let first_result =
    { examples = [ { name = "Passing example"; f = passing_test } ] }
    |> run |> is_success
  in
  expect first_result be_true;
  let second_result =
    { examples = [ { name = "Failing example"; f = failing_test } ] }
    |> run |> is_success
  in
  expect second_result be_false
