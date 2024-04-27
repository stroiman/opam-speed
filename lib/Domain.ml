type test_function = unit -> unit
type example = { name : string; f : test_function }
type t = { examples : example list }

module Context = struct
  let empty = { examples = [] }
end
