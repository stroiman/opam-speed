module type DOMAIN = sig
  type context
  type test_result
  type 'a test_function = 'a -> test_result

  type 'a example = {
    name: string;
    focus: bool;
    f: 'a test_function;
  }

  type 'a t = {
    name: string option;
    child_groups: 'a t list;
    examples: 'a example list;
    has_focused: bool;
  }
end

module type TEST_RESULT = sig
  type t
end

module Make (R : TEST_RESULT) = struct
  type context = unit
  type test_result = R.t
  type 'a test_function = 'a -> test_result

  type 'a example = {
    name: string;
    focus: bool;
    f: 'a test_function;
  }

  type 'a t = {
    name: string option;
    child_groups: 'a t list;
    examples: 'a example list;
    has_focused: bool;
  }

  module Context = struct
    let empty =
      { name= None; child_groups= []; examples= []; has_focused= false }
    ;;
  end

  let empty = Context.empty
end

module MakeFunctions (D : DOMAIN) = struct
  open D

  let empty = { name= None; child_groups= []; examples= []; has_focused= false }

  (* D.empty *)
  let make_suite ?name () = { empty with name }
  let make name = make_suite ~name ()

  let add_example ?(focus = false) name f ctx =
    {
      ctx with
      examples= { name; focus; f } :: ctx.examples;
      has_focused= ctx.has_focused || focus;
    }
  ;;

  let add_child_group child ctx =
    {
      ctx with
      child_groups= child :: ctx.child_groups;
      has_focused= ctx.has_focused || child.has_focused;
    }
  ;;

  let add_context name f =
    let child_group = f (make_suite ~name ()) in
    add_child_group child_group
  ;;

  let get_example_count { examples; _ } = List.length examples
end

module SyncTestResult = struct
  type t = unit
end

module LwtTestResult = struct
  type t = unit Lwt.t
end

module MakeComplex (T : TEST_RESULT) = struct
  module Dom = Make (T)
  include Dom
  include MakeFunctions (Dom)
end

module Sync = MakeComplex (SyncTestResult)
module LwtDomain = MakeComplex (LwtTestResult)
include Sync
