exception TestError

let passing_test () = ()
let failing_test () = raise TestError

let addExample ?(name = "_anonymous test_") f c =
  Speed.Domain.{ examples= { name; f } :: c.examples }
;;

let add_failing_example ?name = addExample ?name failing_test
let add_passing_example ?name = addExample ?name passing_test
let null_formatter = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())
