open Domain

let test name f = { name; f }
let parse examples = { Context.empty with examples }
let root_suite = ref Context.empty

let register examples =
  root_suite := { !root_suite with examples= examples @ !root_suite.examples }
;;
