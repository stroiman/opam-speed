type 'a assertion_result = 'a option

exception AssertionError
exception FormattedAssertionError of (Format.formatter -> unit)

let match_success x = Ok x
let match_failure x = Error (`AssertionError x)

let equality_failure expected actual pp =
  Error
    (`AssertionErrorWithFormat
      (fun format ->
        Format.fprintf format "Expected: @{<green>%a@}@,Actual: @{<red>%a@}" pp
          expected pp actual
      )
      )
;;

let be_true = function
  | true -> match_success true
  | false -> match_failure ()
;;

let be_false = function
  | false -> match_success false
  | true -> match_failure ()
;;

let equal_int expected actual =
  match Int.equal actual expected with
  | true -> Ok actual
  | false -> equality_failure expected actual Format.pp_print_int
;;

let equal_string expected actual =
  match String.equal expected actual with
  | true -> match_success actual
  | false -> equality_failure expected actual Format.pp_print_string
;;

let expect ?name actual assertion =
  match assertion actual with
  | Ok _ -> ()
  | Error (`AssertionErrorWithFormat pp) ->
    let errorFormat f =
      Format.fprintf f "@[<v2>@{<bold>Assertion error@}";
      ( match name with
        | Some n -> Format.fprintf f ": %s" n
        | None -> ()
      );
      Format.fprintf f "@,%t@]" pp
    in
    raise (FormattedAssertionError errorFormat)
  | Error _ -> raise AssertionError
;;

let should ?name assertion actual = expect ?name actual assertion
