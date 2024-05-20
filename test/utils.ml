exception TestError

let ( >> ) f g x = g (f x)
let passing_test _ = ()
let failing_test _ = raise TestError
let addExample ?(name = "_anonymous test_") = Speed.Domain.add_example name

let add_child_group ?name spec =
  Speed.Domain.(add_child_group (make_suite ?name () |> spec))

(* { *)
(*   ctx with *)
(*   child_groups= spec { Context.empty with name } :: ctx.child_groups; *)
(* } *)

let add_failing_example ?name = addExample ?name failing_test
let add_passing_example ?name = addExample ?name passing_test

module Null_formatter = struct
  let fmt = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())
end

let make_ref_string_printer s =
  let out b p l =
    let current = !s in
    let appended = String.sub b p l in
    s := current ^ appended
  in
  Format.make_formatter out (fun _ -> ())

let make_string_printer () =
  let s = ref "" in
  let get_string () = !s in
  let printer = make_ref_string_printer s in
  printer, get_string

let make_string_ref_formatter = make_ref_string_printer

let get_printed_string pp =
  let fmt, get = make_string_printer () in
  pp fmt;
  Format.pp_print_flush fmt ();
  get ()

let run_suite_silent = Speed.Runner.run_suite ~fmt:Null_formatter.fmt
