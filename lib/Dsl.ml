open Domain

let test name f ctx = { ctx with examples= { name; f } :: ctx.examples }
let parse specs = specs |> List.fold_left (fun a b -> b a) Context.empty

let context name specs ctx =
  { ctx with child_groups= { (parse specs) with name= Some name } :: ctx.child_groups }
;;

let root_suite = ref Context.empty

let register examples =
  root_suite
  := { !root_suite with child_groups= parse examples :: !root_suite.child_groups }
;;

let root_context name specs = register [ context name specs ]
