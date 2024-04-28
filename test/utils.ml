exception TestError

let passing_test () = ()
let failing_test () = raise TestError

let addExample ?(name = "_anonymous test_") f c =
  Speed.Domain.{ c with examples= { name; f } :: c.examples }
;;

let add_child_group ?name spec ctx =
  Speed.Domain.
    {
      ctx with
      child_groups= spec { Speed.Domain.Context.empty with name } :: ctx.child_groups;
    }
;;

let add_failing_example ?name = addExample ?name failing_test
let add_passing_example ?name = addExample ?name passing_test
let null_formatter = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())

let get_printed_string pp =
  let s = ref "" in
  let out b p l =
    let current = !s in
    let appended = String.sub b p l in
    s := current ^ appended
  in
  let fmt = Format.make_formatter out (fun _ -> ()) in
  pp fmt;
  Format.pp_print_flush fmt ();
  !s
;;

let make_ref_string_printer s =
  let out b p l =
    let current = !s in
    let appended = String.sub b p l in
    s := current ^ appended
  in
  Format.make_formatter out (fun _ -> ())
;;
(* pp fmt; *)
(* Format.pp_print_flush fmt () *)
