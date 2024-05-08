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

module Make (T : DOMAIN) = struct
  open T

  type t = T.t

  let test = add_example
  let it = test
  let parse_to_ctx specs ctx = specs |> List.fold_left (fun a b -> b a) ctx
  let parse specs = parse_to_ctx specs empty
  let context name spec ctx = add_context name (parse_to_ctx spec) ctx
  let root_suite = ref empty

  let register examples =
    root_suite := !root_suite |> add_child_group (parse examples)
  ;;

  let root_context name specs = register [context name specs]
end

module Sync = Make (Domain.Sync)
module LwtDsl = Make (Domain.LwtDomain)
include Sync
