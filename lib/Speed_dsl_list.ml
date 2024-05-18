module Domain = Speed_domain

module Make (T : Domain.DOMAIN) = struct
  open Domain.MakeFunctions (T)

  type t = unit T.t

  let test = add_example
  let it = test
  let parse_to_ctx specs ctx = specs |> List.fold_left (fun a b -> b a) ctx
  let parse specs = parse_to_ctx specs @@ make_suite ()
  let context name spec ctx = add_context name (parse_to_ctx spec) ctx
  let root_suite : t ref = ref (make_suite ())

  let register examples =
    root_suite := !root_suite |> add_child_group (parse examples)
  ;;

  let root_context name specs = register [context name specs]
end

module Sync = Make (Domain.Sync)
module LwtDsl = Make (Domain.LwtDomain)
include Sync
