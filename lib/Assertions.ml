type 'a assertion_result = 'a option

exception AssertionError

let be_true = function true -> Some true | false -> None
let be_false = function false -> Some false | true -> None

let expect actual assertion =
  match assertion actual with Some _ -> () | None -> raise AssertionError
