# Speed docs

This is far from complete documentation, but may help you get started.

> [!WARNING]
> This is very much work in progress. I do strive to keep the "public interface"
> stable, whereas I may change the internal structure at will. There are no .mli
> files to help identify the stable parts.

## Installing

You can't. Git the source, build and install from a local directory:

```sh
dune build
dune install
opam install .
```

Honestly, I know too little about opam if all these steps are necessary.

## Building the structure

### Constrcuting the domain (not recommended)

The structure of tests, and groups of tests are represented by types in the 
`Speed.Domain` module. These are parameterised on test output, and two variants
exist; one for synchronous tests, having the form of `unit -> unit`, and one
for concurrent tests using Lwt, having the form of `unit -> unit promise`.

### DSL

This constructs the DSL using lists. This is the approach I have been using 
before, and provides a lot of power.

Where e.g. some unit tests frameworks in some imperative programming languages
need explicit support if you want data-drive tests, using lists as input removes
this need. You can just use list map function, e.g.

```ocaml
describe "Email validator"
  [("jd@example.com", true);
   ("jd@example", false);
   ("@example.com", false)]
  |> List.map(fun (email, valid) -> 
    it (Format.sprintf "Should validate '%s': %s" email valid) (fun _ ->
      email |> is_valid |> should ~name:email equal_bool valid
    )
  )
```

> [!NOTE]
> Currently, the DSL constructs domain objects directly, but in my previuos 
> projects, FSpec and Respect, this would be a set of instructions represented
> by variants, to construct the domain.
> 
> I might very will do the same here, but I will strive to keep the API 
> compatible.

### Effect DSL

This uses OCaml 5 effects to produce a more _imperative like_ structure.

> [!NOTE]
> My primary reason for preferring this approach is that it formats nicer with 
> ocamlformat, as I can get it to preserve line breaks between tests; making it
> much more readable

```ocaml
open Speed.Dsl.Effect_dsl

(* important, must be a function, so the parser can consume the effects *)
let build_suite () =
  context "Root context" (fun _ ->
    test "Some test" (fun _ ->
      ()
    );

    test "Some other test" (fun _ ->
      ()
    )
  )

(* Construct the domain, consuming the effects *)
let suite = parser build_suite

## Root suite

This is the only piece of mutable code in the library. This has one `suite ref`
and there are functions that appends to this during building the test structure.
(it's not called `suite`, but here it reads better than `t ref`). 

This is not necessary, you can build and run the suite explicitly (the test 
suite of speed itself does exactly that)

There is a separate suite for sync tests, and for lwt tests. 

## Running tests

`Speed.Runner` exposes `run_root_suites` which executes both the sync root suite
and the lwt root suite.

But if you explicitly build the suite, there are functions inside to run them
explicitly; follow the code to find those.

### Organising in multiple files

> [!WARNING]
> If your tests are split into several files, be sure that the test files are
> evaluated _before_ the file running the root suites

One option is to wrap the suite construction in an explicit function

```ocaml
(* test_suite_1.ml *)
let setup () =
  run_root ( (* ... *) )

(* test_main.ml *)
Test_suite_1.setup ()

Speed.Runner.run_root_suites ()
```

Another option, use `open!`

```ocaml
(* test_suite_1.ml *)
run_root ( (* ... *) )

(* test_main.ml *)
open! Test_suite_1

Speed.Runner.run_root_suites ()
```

## Assertions

The library contains an assertion framework. I generally believe that assertions
and tests should be kept separate, but I needed something a little more flexible
than what I could find, so this library is currently the testing ground for
an assertion library

### Concept

The concept is based on _a matcher_. A matcher can verify that a value meets a
specification. Currently, the matcher has the format `'a -> ('b, 'c) result`,
but the will likely change into a record containing more information, such as a
descriptive neme.

```ocaml
(* Running with expect syntax. An optional name can be added *)
expect "foo" @@ equal_string "foo"
expect ~name:"First assertion" "foo" @@ equal_string "foo"
(* or *)
expect "foo" (equal_string "foo")

(* Running with should syntax. An optional name can be added *)
"foo" |> should @@ equal_string "foo"
"foo" |> expect ~name:"First assertion" "foo" @@ equal_string "foo"
```

### ppx_generator

There is a ppx generator for creating matchers for parts 

To avoid failing tests when you refactor code, a test should only be concerned
about the single aspect it's testing. The ppx generators allow you to match just
the fields relevant.

This can be combines with ppx_import to keep test code away from your production
code.

```ocaml
(* lib/domain.ml *)
type account = {
  id: account_id;
  email: string;
  password_hash: string;
}

(* test/domain_test.ml *)
type account = [%import: Lib.Domain.account] [@@deriving matcher]

test "New account has right email" (fun _ ->
  let account = make_account ~email:"jd@example.com" ~password:"1234" () in
  account |> should @@ match_account ~email:(match_string "jd@example.coM")
)

test "New account has right email" (fun _ ->
  let account = make_account ~email:"jd@example.com" ~password:"1234" () in
  account |> should @@ match_account ~password_hash:(match_password "jd@example.coM")
)
```
