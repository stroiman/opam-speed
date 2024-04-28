open Domain

let test name f = { name; f }
let parse examples = { examples }
let root_suite = ref Context.empty
let register examples = root_suite := { examples= examples @ !root_suite.examples }
