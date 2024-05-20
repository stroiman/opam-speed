module Domain = Speed_domain

module type ROOT_SUITE = sig
  type t

  val root_suite : t ref
end

module Make (D : Domain.DOMAIN) (RootSuite : ROOT_SUITE with type t = unit D.t) =
struct
  open Domain.MakeFunctions (D)
  open Effect

  type _ Effect.t += Op : (RootSuite.t -> RootSuite.t) -> unit t

  let run (f : unit -> unit) (ctx : RootSuite.t) =
    let open Effect.Shallow in
    let rec loop
      : type a. (a, unit) continuation -> a -> RootSuite.t -> RootSuite.t
      =
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

  let parse f = run f @@ make_suite ()
  let run_root f = RootSuite.root_suite := run f !RootSuite.root_suite
  let test ?focus name f = Effect.perform (Op (add_example ?focus name f))
  let it = test

  let context name ?focus specs =
    perform (Op (make_suite ?focus ~name () |> run specs |> add_child_group))

  let root_context ?focus name f = run_root (fun _ -> context ?focus name f)
end

module Sync = Make (Domain.Sync) (Speed_dsl_list.Sync)
module LwtEffectDsl = Make (Domain.LwtDomain) (Speed_dsl_list.LwtDsl)
include Sync
