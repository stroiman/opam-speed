type 'a assertion_result = 'a option

exception AssertionError

let be_true = function
  | true -> Some true
  | false -> None
;;

let be_false = function
  | false -> Some false
  | true -> None
;;

let equal_int expected actual =
  match Int.equal expected actual with
  | true -> Some actual
  | false -> None
;;

let expect actual assertion =
  match assertion actual with
  | Some _ -> ()
  | None -> raise AssertionError
;;
