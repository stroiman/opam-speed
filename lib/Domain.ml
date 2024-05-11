module type DOMAIN = sig
  type test_result
  type 'a test_function = 'a -> test_result

  type 'a example = {
    name: string;
    focus: bool;
    f: 'a test_function;
  }

  type 'a t =
    | Context : {
        name: string option;
        child_groups: 'a t list;
        examples: 'a example list;
        has_focused: bool;
      }
        -> 'a t
end

module type TEST_RESULT = sig
  type t
end

module Make (R : TEST_RESULT) = struct
  type test_result = R.t
  type 'a test_function = 'a -> test_result

  type 'a example = {
    name: string;
    focus: bool;
    f: 'a test_function;
  }

  type 'a t =
    | Context : {
        name: string option;
        child_groups: 'a t list;
        examples: 'a example list;
        has_focused: bool;
      }
        -> 'a t

  type u

  type gen_context =
    | Create : {
        context: 'a t;
        fixture: unit -> 'a;
      }
        -> gen_context
end

module MakeFunctions (D : DOMAIN) = struct
  open D

  let empty =
    Context { name= None; child_groups= []; examples= []; has_focused= false }
  ;;

  (* D.empty *)
  let make_suite ?name () =
    Context { name; child_groups= []; examples= []; has_focused= false }
  ;;

  let make name = make_suite ~name ()

  let add_example ?(focus = false) name f = function
    | Context ctx ->
      Context
        {
          ctx with
          examples= { name; focus; f } :: ctx.examples;
          has_focused= ctx.has_focused || focus;
        }
  ;;

  let add_child_group : 'a. 'a t -> 'a t -> 'a t =
    fun child parent ->
    match child, parent with
    | Context c, Context ctx ->
      Context
        {
          ctx with
          child_groups= Context c :: ctx.child_groups;
          has_focused= ctx.has_focused || c.has_focused;
        }
  ;;

  let add_context name f =
    let child_group = f (make_suite ~name ()) in
    add_child_group child_group
  ;;

  let get_example_count grp =
    let rec iter acc (Context { examples; child_groups; _ }) =
      Base.List.fold_left
        ~init:(acc + List.length examples)
        ~f:iter child_groups
    in
    iter 0 grp
  ;;

  let child_group_count (Context { child_groups; _ }) = List.length child_groups
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
