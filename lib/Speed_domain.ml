(* type metadata = Speed_metadata.metadata.t *)
module Metadata = Speed_metadata

module TestInput = struct
  type 'a t = {
    metadata: Metadata.t list;
    subject: 'a;
  }

  let get_metadata { metadata; _ } = metadata
end

type 'a test_input = 'a TestInput.t

module type DOMAIN = sig
  type test_result
  type 'a test_function = 'a test_input -> test_result

  type 'a example = {
    name: string;
    focus: bool;
    metadata: Metadata.t list;
    f: 'a test_function;
  }

  type 'a t = {
    name: string option;
    focus: bool;
    child_groups: 'a child_suite list;
    metadata: Metadata.t list;
    examples: 'a example list;
    has_focused: bool;
  }

  and 'a child_suite =
    | Child : {
        child: 'b t;
        setup: 'a test_input -> 'b;
      }
        -> 'a child_suite
    | Context : { child: 'a t } -> 'a child_suite
end

module type TEST_RESULT = sig
  type t
end

module Make (R : TEST_RESULT) = struct
  type test_result = R.t
  type 'a test_function = 'a test_input -> test_result

  type 'a example = {
    name: string;
    focus: bool;
    metadata: Metadata.t list;
    f: 'a test_function;
  }

  module Example = struct
    let has_focus ex = ex.focus
  end

  type 'a t = {
    name: string option;
    focus: bool;
    child_groups: 'a child_suite list;
    metadata: Metadata.t list;
    examples: 'a example list;
    has_focused: bool;
  }

  and 'a child_suite =
    | Child : {
        child: 'b t;
        setup: 'a test_input -> 'b;
      }
        -> 'a child_suite
    | Context : { child: 'a t } -> 'a child_suite
end

module MakeFunctions (D : DOMAIN) = struct
  open D

  let empty =
    {
      name= None;
      focus= false;
      child_groups= [];
      metadata= [];
      examples= [];
      has_focused= false;
    }

  let make_suite ?name ?(focus = false) ?(metadata = []) () =
    { empty with name; metadata; focus }

  let make name = make_suite ~name ()

  let add_example ?(focus = false) ?(metadata = []) name f = function
    | ctx ->
      {
        ctx with
        examples= { name; focus; f; metadata } :: ctx.examples;
        has_focused= ctx.has_focused || focus;
      }

  let add_child child parent =
    match child with
    | Child { child= { has_focused; focus; _ }; _ } ->
      {
        parent with
        child_groups= child :: parent.child_groups;
        has_focused= parent.has_focused || has_focused || focus;
      }
    | Context { child= { has_focused; focus; _ } } ->
      {
        parent with
        child_groups= child :: parent.child_groups;
        has_focused= parent.has_focused || has_focused || focus;
      }

  (* If I add ?setup as input, it gets inferred to `a test_input -> a` instead
     of `a test_input -> b` - but in the one below, where it's not optional,
     it's inferred correctly. Annoying *)
  let add_child_group child = add_child (Context { child })

  let add_child_group_with_setup ~setup child =
    add_child (Child { child; setup })

  let add_fixture ?name ?focus ?metadata ~setup f =
    let child = f (make_suite ?name ?focus ?metadata ()) in
    add_child (Child { child; setup })

  let add_context ?focus ?metadata name f =
    let child_group = f (make_suite ~name ?focus ?metadata ()) in
    add_child_group child_group

  let get_example_count grp =
    let rec iter_group : 'a. int -> 'a t -> int =
      fun acc -> function
      | { examples; child_groups; _ } ->
        List.fold_left iter_mixed (acc + List.length examples) child_groups
    and iter_mixed acc x =
      x
      |> function
      | Child { child; _ } -> iter_group acc child
      | Context { child } -> iter_group acc child
    in
    iter_group 0 grp

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
