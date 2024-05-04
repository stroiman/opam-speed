module type TEST_RESULT = sig
  type t
end

module Make (R : TEST_RESULT) = struct
  type context = unit
  type test_result = R.t
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

  module Context = struct
    let empty = { name= None; child_groups= []; examples= [] }
  end

  let empty = Context.empty

  let add_example name f ctx =
    { ctx with examples= { name; f } :: ctx.examples }
  ;;

  let make name = { empty with name= Some name }

  let add_context name f ctx =
    { ctx with child_groups= f (make name) :: ctx.child_groups }
  ;;
end

module SyncTestResult = struct
  type t = unit
end

module LwtTestResult = struct
  type t = unit Lwt.t
end

module Sync = Make (SyncTestResult)
module LwtDomain = Make (LwtTestResult)
include Sync
