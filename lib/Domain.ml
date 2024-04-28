type test_function = unit -> unit

type example = {
  name: string;
  f: test_function;
}

type t = {
  name: string option;
  child_groups: t list;
  examples: example list;
}

module Context = struct
  let empty = { name= None; child_groups= []; examples= [] }
end
