module type ROOT_SUITE = sig
  type t

  val root_suite : t ref
end

module Make (D : Dsl.DOMAIN) (RootSuite : ROOT_SUITE with type t = D.t) = struct
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
  let add_test name f ctx = { ctx with examples= { name; f } :: ctx.examples }
  let test name f = Effect.perform (Op (add_test name f))
  let it = test

  let add_child_context child ctx =
    { ctx with child_groups= child :: ctx.child_groups }
  ;;

  let context name specs =
    let ctx = { D.empty with name= Some name } in
    let result = run specs ctx in
    perform (Op (add_child_context result))
  ;;
end

module Sync = Make (Domain.Sync) (Dsl.Sync)
module Lwt = Make (Domain.Lwt) (Dsl.Lwt)
include Sync
