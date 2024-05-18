module Domain = Speed_domain

type metadata = Speed_metadata.t

module type ROOT_SUITE = sig
  type t

  val root_suite : t ref
end

module Make (D : Domain.DOMAIN) (RootSuite : ROOT_SUITE with type t = unit D.t) =
struct
  open Domain.MakeFunctions (D)

  type 'a builder = {
    context: ?metadata:metadata list -> string -> ('a builder -> unit) -> unit;
    fixture:
      'b.
      ?metadata:metadata list ->
      setup:('a Domain.test_input -> 'b) ->
      string ->
      ('b builder -> unit) ->
      unit;
    test: ?metadata:metadata list -> string -> 'a D.test_function -> unit;
  }

  module MakeTypes (C : sig
      type t
    end) =
  struct
    type _ Effect.t += Op : (C.t D.t -> C.t D.t) -> unit Effect.t
  end

  let rec run : type a. (a builder -> unit) -> a D.t -> a D.t =
    fun (f : a builder -> unit) (ctx : a D.t) : a D.t ->
    let open Effect in
    let open Effect.Shallow in
    let open MakeTypes (struct
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
    let fixture ?metadata ~setup name specs =
      perform (Op (add_fixture ?metadata ~name ~setup @@ run specs))
    in
    let context ?metadata name specs =
      perform (Op (add_context ?metadata name (run specs)))
    in
    let test ?metadata name f = perform (Op (add_example ?metadata name f)) in

    loop (fiber f) { fixture; context; test } ctx
  ;;

  let parse (f : unit builder -> unit) = run f @@ make_suite ()
  let run_root f = RootSuite.root_suite := run f !RootSuite.root_suite
  let root_context name f = run_root (fun s -> s.context name f)
end

module Sync = Make (Domain.Sync) (Speed_dsl_list.Sync)
module LwtEffectDsl = Make (Domain.LwtDomain) (Speed_dsl_list.LwtDsl)
include Sync
