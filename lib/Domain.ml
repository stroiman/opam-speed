module type TEST_RESULT = sig
  type t
end

module Make (R : TEST_RESULT) = struct
  type context = unit
  type test_result = R.t
  type test_function = context -> test_result

  type example = {
    name: string;
    focus: bool;
    f: test_function;
  }

  type t = {
    name: string option;
    child_groups: t list;
    examples: example list;
    has_focused: bool;
  }

  module Context = struct
    let empty =
      { name= None; child_groups= []; examples= []; has_focused= false }
    ;;
  end

  let empty = Context.empty

  let add_example ?(focus = false) name f ctx =
    {
      ctx with
      examples= { name; focus; f } :: ctx.examples;
      has_focused= ctx.has_focused || focus;
    }
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
