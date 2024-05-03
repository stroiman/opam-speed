type 'a assertion_result = 'a option

module AssertionResult = struct
  type ('a, 'b) t = ('a, 'b) result

  let bind ~f x = Result.bind x f
  let map = Result.map
end

type print = Format.formatter -> unit

exception AssertionError
exception FormattedAssertionError of (Format.formatter -> unit)

let match_success x = Ok x

let match_failure ?(pp : print option) x =
  match pp with
  | None -> Error (`AssertionError x)
  | Some x -> Error (`AssertionErrorWithFormat x)
;;

let equality_failure expected actual pp =
  match_failure
    ~pp:
      (Format.dprintf "Expected: @{<green>%a@}@,Actual: @{<red>%a@}" pp expected
         pp actual
      )
    ()
;;

let be_true = function
  | true -> match_success true
  | false -> match_failure ()
;;

let be_ok = function
  | Ok x -> match_success x
  | Error _ -> match_failure ()
;;

let be_error = function
  | Error x -> match_success x
  | Ok _ -> match_failure ()
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

let contain substring actual =
  match Base.String.is_substring ~substring actual with
  | true -> match_success actual
  | false ->
    equality_failure
      (Format.sprintf "string containing '%s'" substring)
      actual Format.pp_print_string
;;

let run_matcher matcher actual = matcher actual

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
