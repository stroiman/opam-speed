type test_function = unit -> unit
type example = { name : string; f : test_function }
type context = { examples : example list }
