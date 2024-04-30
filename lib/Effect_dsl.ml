open Dsl
open Domain
open Effect

type _ Effect.t += Op : (Domain.t -> Domain.t) -> unit t

let run (f : unit -> unit) (ctx : Domain.t) =
  let open Effect.Shallow in
  let rec loop : type a. (a, unit) continuation -> a -> Domain.t -> Domain.t =
   fun k v ctx ->
    continue_with k v
      {
        retc= (fun _ -> ctx)
      ; exnc= raise
      ; effc=
          (fun (type b) (eff : b t) ->
            match eff with
            | Op n -> Some (fun (k : (b, _) continuation) -> loop k () (n ctx))
            | _ -> None)
      }
  in
  loop (fiber f) () ctx

let parse f = run f Context.empty
let run_root f = root_suite := run f !root_suite
let test name f = Effect.perform (Op (test name f))

let add_child_context child ctx =
  { ctx with child_groups= child :: ctx.child_groups }

let context name specs =
  let ctx = { Domain.Context.empty with name= Some name } in
  let result = run specs ctx in
  perform (Op (add_child_context result))
