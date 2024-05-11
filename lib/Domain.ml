module type DOMAIN = sig
  type context
  type test_result
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

  val empty : t
  val add_example : ?focus:bool -> string -> test_function -> t -> t
  val add_context : string -> (t -> t) -> t -> t
  val add_child_group : t -> t -> t
end

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

  let add_child_group child ctx =
    {
      ctx with
      child_groups= child :: ctx.child_groups;
      has_focused= ctx.has_focused || child.has_focused;
    }
  ;;

  let add_context name f =
    let child_group = f (make name) in
    add_child_group child_group
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
