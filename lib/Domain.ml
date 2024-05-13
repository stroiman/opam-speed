module type DOMAIN = sig
  type test_result
  type 'a test_function = 'a -> test_result

  type 'a example = {
    name: string;
    focus: bool;
    f: 'a test_function;
  }

  type 'a t = {
    name: string option;
    child_groups: 'a child_suite list;
    examples: 'a example list;
    has_focused: bool;
  }

  and 'a child_suite = Child : { child: 'a t } -> 'a child_suite
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

  type 'a t = {
    name: string option;
    child_groups: 'a child_suite list;
    examples: 'a example list;
    has_focused: bool;
  }

  and 'a child_suite = Child : { child: 'a t } -> 'a child_suite
end

module MakeFunctions (D : DOMAIN) = struct
  open D

  let empty = { name= None; child_groups= []; examples= []; has_focused= false }

  (* D.empty *)
  let make_suite ?name () =
    { name; child_groups= []; examples= []; has_focused= false }
  ;;

  let make name = make_suite ~name ()

  let add_example ?(focus = false) name f = function
    | ctx ->
      {
        ctx with
        examples= { name; focus; f } :: ctx.examples;
        has_focused= ctx.has_focused || focus;
      }
  ;;

  let add_child_group child parent =
    match child, parent with
    | c, ctx ->
      {
        ctx with
        child_groups= Child { child } :: ctx.child_groups;
        has_focused= ctx.has_focused || c.has_focused;
      }
  ;;

  let add_context name f =
    let child_group = f (make_suite ~name ()) in
    add_child_group child_group
  ;;

  let get_example_count grp =
    let rec iter_group : 'a. int -> 'a t -> int =
      fun acc -> function
      | { examples; child_groups; _ } ->
        Base.List.fold_left
          ~init:(acc + List.length examples)
          ~f:iter_mixed child_groups
    and iter_mixed : 'a. int -> 'a child_suite -> int =
      fun acc x ->
      x
      |> function
      | Child { child } -> iter_group acc child
    in
    iter_group 0 grp
  ;;

  let child_group_count { child_groups; _ } = List.length child_groups
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
