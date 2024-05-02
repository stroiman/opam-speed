open Speed.Assertions
open Speed.Effect_dsl

type test_type = {
  a: string;
  b: int;
}
[@@deriving matcher]

type t = {
  x: string;
  y: string;
}
[@@deriving matcher]

let match_test = test_type_matcher

let _match_test ?a ?b actual =
  let ( let* ) = Result.bind in
  let* _ =
    Option.fold ~none:(Ok ()) ~some:(fun v -> v actual.a |> Result.map ignore) a
  in
  let* _ =
    Option.fold ~none:(Ok ()) ~some:(fun v -> v actual.b |> Result.map ignore) b
  in
  Ok ()
;;

let be_ok = function
  | Ok _ -> Ok ()
  | Error _ -> Error `AssertionError
;;

let be_error = function
  | Ok _ -> Error `AssertionError
  | Error _ -> Ok ()
;;

run_root (fun _ ->
  context "Record matcher" (fun _ ->
    it "Should succeed when property matches" (fun _ ->
      let matcher = match_test ~a:(equal_string "Actual") in
      matcher { a= "Actual"; b= 42 } |> should be_ok
    );

    it "Should fail when property doesn't match" (fun _ ->
      let matcher = match_test ~a:(equal_string "Wrong") in
      matcher { a= "Actual"; b= 42 } |> should be_error
    );

    it "Should succeed when both properties match" (fun _ ->
      let matcher = match_test ~a:(equal_string "Actual") ~b:(equal_int 42) in
      matcher { a= "Actual"; b= 42 } |> should be_ok
    );

    it "Should fail when any property mismatch" (fun _ ->
      let matcher = match_test ~a:(equal_string "Actual") ~b:(equal_int 43) in
      matcher { a= "Actual"; b= 42 } |> should be_error
    );

    it "Should just be called 'have' for a type named 't'"
      [%f { x= "x"; y= "y" } |> should (have ~x:(equal_string "x"))]
  )
)
