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

let equality_failure expected actual pp =
  match_failure
    ~pp:
      (Format.dprintf "Expected: @{<green>%a@}@,Actual: @{<red>%a@}" pp expected
         pp actual
      )
    ()

let be_true = function
  | true -> match_success true
  | false -> match_failure ()

let be_ok = function
  | Ok x -> match_success x
  | Error _ -> match_failure ()

let be_error = function
  | Error x -> match_success x
  | Ok _ -> match_failure ()

let be_false = function
  | false -> match_success false
  | true -> match_failure ()

let equal_int expected actual =
  match Int.equal actual expected with
  | true -> Ok actual
  | false -> equality_failure expected actual Format.pp_print_int

let equal_string expected actual =
  match String.equal expected actual with
  | true -> match_success actual
  | false -> equality_failure expected actual Format.pp_print_string

let is_substring actual search =
  let c = String.get search 0 in
  let l = String.length search in
  let max_i = String.length actual - l in
  let rec iter index =
    match String.index_from_opt actual index c with
    | None -> false
    | Some i ->
      if i > max_i
      then false
      else if String.sub actual i l |> String.equal search
      then true
      else iter (index + 1)
  in
  iter 0

let contain substring actual =
  match is_substring actual substring with
  | true -> match_success actual
  | false ->
    equality_failure
      (Format.sprintf "string containing '%s'" substring)
      actual Format.pp_print_string

let run_matcher matcher actual = matcher actual

let expect ?name actual assertion =
  let print_header f =
    Format.fprintf f "@[<v2>@{<bold>@{<orange>Assertion error@}@}";
    match name with
    | Some n -> Format.fprintf f ": %s" n
    | None -> ()
  in
  match assertion actual with
  | Ok _ -> ()
  | Error (`AssertionErrorWithFormat pp) ->
    let errorFormat f =
      print_header f;
      Format.fprintf f "@,%t@]" pp
    in
    raise (FormattedAssertionError errorFormat)
  | Error _ ->
    let errorFormat f =
      print_header f;
      Format.fprintf f "@]"
    in
    raise (FormattedAssertionError errorFormat)

let ( >=> ) m1 m2 actual = Result.bind (m1 actual) m2
let should ?name assertion actual = expect ?name actual assertion
