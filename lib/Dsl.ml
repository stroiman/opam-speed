module type DOMAIN = sig
  type context
  type test_result
  type test_function = context -> test_result

  type example = {
    name: string;
    f: test_function;
  }

  type t = {
    name: string option;
    child_groups: t list;
    examples: example list;
  }

  val empty : t
end

module Make (T : Domain.DOMAIN) = struct
  open T

  type t = T.t

  let test name f ctx = { ctx with examples= { name; f } :: ctx.examples }
  let it = test
  let parse specs = specs |> List.fold_left (fun a b -> b a) empty

  let context name specs ctx =
    {
      ctx with
      child_groups= { (parse specs) with name= Some name } :: ctx.child_groups;
    }
  ;;

  let root_suite = ref empty

  let register examples =
    root_suite
    := {
         !root_suite with
         child_groups= parse examples :: !root_suite.child_groups;
       }
  ;;

  let root_context name specs = register [context name specs]
end

module Sync = Make (Domain.Sync)
module Lwt = Make (Domain.Lwt)
include Sync
