module type ROOT_SUITE = sig
  type t

  val root_suite : t ref
end

module Make (D : Domain.DOMAIN) (RootSuite : ROOT_SUITE with type t = D.t) =
struct
  open D
  open Effect

  type _ Effect.t += Op : (D.t -> D.t) -> unit t

  let run (f : unit -> unit) (ctx : D.t) =
    let open Effect.Shallow in
    let rec loop : type a. (a, unit) continuation -> a -> D.t -> D.t =
      fun k v ctx ->
      continue_with k v
        {
          retc= (fun _ -> ctx);
          exnc= raise;
          effc=
            (fun (type b) (eff : b t) ->
              match eff with
              | Op n -> Some (fun (k : (b, _) continuation) -> loop k () (n ctx))
              | _ -> None
            );
        }
    in
    loop (fiber f) () ctx
  ;;

  let parse f = run f D.empty
  let run_root f = RootSuite.root_suite := run f !RootSuite.root_suite
  let test ?focus name f = Effect.perform (Op (D.add_example ?focus name f))
  let it = test
  let add_child_context = add_child_group

  let context name specs =
    let ctx = { D.empty with name= Some name } in
    let result = run specs ctx in
    perform (Op (add_child_context result))
  ;;

  let root_context name f = run_root (fun _ -> context name f)
end

module Sync = Make (Domain.Sync) (Dsl.Sync)
module LwtEffectDsl = Make (Domain.LwtDomain) (Dsl.LwtDsl)
include Sync
