module type ROOT_SUITE = sig
  type t

  val root_suite : t ref
end

module Make (D : Domain.DOMAIN) (RootSuite : ROOT_SUITE with type t = unit D.t) =
struct
  open Domain.MakeFunctions (D)

  type 'a builder = {
    context: string -> ('a builder -> unit) -> unit;
    fixture:
      'b.
      setup:('a Domain.test_input -> 'b) ->
      string ->
      ('b builder -> unit) ->
      unit;
    test: ?metadata:Domain.metadata list -> string -> 'a D.test_function -> unit;
  }

  module MakeInner (C : sig
      type t
    end) =
  struct
    type _ Effect.t += Op : (C.t D.t -> C.t D.t) -> unit Effect.t
  end

  let rec run : type a. (a builder -> unit) -> a D.t -> a D.t =
    fun (f : a builder -> unit) (ctx : a D.t) : a D.t ->
    let open Effect in
    let open Effect.Shallow in
    let open MakeInner (struct
        type t = a
      end) in
    let rec loop : type c. (c, _) continuation -> c -> 'a D.t -> 'a D.t =
      fun k v ctx ->
      continue_with k v
        {
          retc= (fun _ -> ctx);
          exnc= raise;
          effc=
            (fun (type b) (eff : b Effect.t) ->
              match eff with
              | Op n -> Some (fun (k : (b, _) continuation) -> loop k () (n ctx))
              | _ -> None
            );
        }
    in
    loop (fiber f)
      {
        fixture=
          (fun (type b)
            ~(setup : a Domain.test_input -> b)
            name
            (specs : b builder -> unit) ->
            let op =
              make name |> run specs |> add_child_group_with_setup ~setup
            in
            perform (Op op)
          );
        context=
          (fun name specs ->
            perform (Op (make name |> run specs |> add_child_group))
          );
        test=
          (fun ?metadata name f ->
            Effect.perform (Op (add_example ?metadata name f))
          );
      }
      ctx
  ;;

  let parse (f : unit builder -> unit) = run f @@ make_suite ()
end

module Sync = Make (Domain.Sync) (Dsl.Sync)
module LwtEffectDsl = Make (Domain.LwtDomain) (Dsl.LwtDsl)
include Sync
