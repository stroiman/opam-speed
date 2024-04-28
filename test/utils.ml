exception TestError

let passing_test () = ()
let failing_test () = raise TestError

let addExample ?(name = "_anonymous test_") f c =
  Speed.Domain.{ examples= { name; f } :: c.examples }
;;

let null_formatter = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())
