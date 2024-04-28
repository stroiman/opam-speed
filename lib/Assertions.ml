type 'a assertion_result = 'a option

exception AssertionError
exception FormattedAssertionError of (Format.formatter -> unit)

let match_success x = Ok x
let match_failure x = Error (`AssertionError x)

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
  | false ->
    Error
      (`AssertionErrorWithFormat
        (fun format () ->
          Format.fprintf
            format
            "Expected: @{<green>%d@}@,Actual: @{<red>%d@}"
            expected
            actual))
;;

let equal_string expected actual =
  match String.equal expected actual with
  | true -> match_success actual
  | false -> match_failure actual
;;

let expect ?name actual assertion =
  match assertion actual with
  | Ok _ -> ()
  | Error (`AssertionErrorWithFormat pp) ->
    let errorFormat f =
      Format.fprintf f "@[<v2>@{<bold>Assertion error@}";
      (match name with
       | Some n -> Format.fprintf f ": %s" n
       | None -> ());
      Format.fprintf f "@,%a@]" pp ()
    in
    raise (FormattedAssertionError errorFormat)
  | Error _ -> raise AssertionError
;;
