module type TEST_RESULT = sig
  type t
end

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
end

module SyncTestResult = struct
  type t = unit
end

module LwtTestResult = struct
  type t = unit Lwt.t
end

module Sync = Make (SyncTestResult)
module Lwt = Make (LwtTestResult)
include Sync
