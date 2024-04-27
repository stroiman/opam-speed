exception TestError

let passing_test () = ()
let failing_test () = raise TestError
