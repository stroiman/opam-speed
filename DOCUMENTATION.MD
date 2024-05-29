# Speed docs

This is far from complete documentation, but may help you get started.

> [!WARNING]
> This is very much work in progress. I do strive to keep the "public interface"
> stable, whereas I may change the internal structure at will. There are no .mli
> files to help identify the stable parts.

## Installing

There is an early version available through opam, but because the release cycle
for opam packages may take days, I don't publish that often, so your best bet
is just to get the code from github, and build locally.

```sh
make install
```

Or; if you don't have `make` installed:

```sh
dune build
dune install
opam install .
```

## Writing tests

There are two different ways of building a test suite, the list-DSL, and 
effect based DSL.

### List DSL

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

### Effect DSL

This uses OCaml 5 effects to produce a more _imperative like_ structure.

> [!NOTE]
> My primary reason for preferring this approach is that it formats nicer with
> ocamlformat, as I can get it to preserve line breaks between tests; making it
> much more readable

```ocaml
open Speed.Dsl.Effect
;;

(* important, must be a function, so the parser can consume the effects *)
let build_suite () =
  context "Root context" (fun s ->
    s.test "Some test" (fun s ->
      ()
    );

    s.test "Some other test" (fun s ->
      ()
    )
  )

(* Construct the domain, consuming the effects *)
let suite = parser build_suite
```

### Light Syntax

A variation exists of the Effect DSL that has a slightly lighter syntax, but 
this does not allow for all features to work.

```ocaml
open Speed.Dsl.Effect.Light
;;

root_context "Context" (fun _ ->
  test "Test" (fun _ -> 
    (* ... *) 
  );

  context "Child" (fun _ -> 
    test "child test" (fun _ -> 
      (* ... *) 
    );

    (* ... *) 
  )
)
```

### Testing async code

There is a child module, `LwtEffectDsl` that expects test functions to return
an `unit promise` instead of a `unit`, relieving the need to wait for a result.

```ocaml
open Speed.Dsl.Effect.LwtEffectDsl
open Lwt.Syntax
;;

root_context "Root context" (fun s ->
  s.test "Some test" (fun s ->
    let+ result = do_something_returning_lwt_promise in
    result |> should match_expectation
  );

  s.test "Some other test" (fun s ->
    Let.return ()
  )
)
```

> [!WARNING] 
> In the current version of the code, the Lwt test runner will run _all_ tests
> inside a _single group_ in parallel, but the groups themselves will run
> sequentually. I.e. if two sibling groups each contain 5 tests, the first 5
> tests will run in parallel. When they all complete, the last 5 tests will run
> in parallel.
>
> This is ONLY the case for Lwt tests, tests using the sync suite runs in 
> sequence. A suite that contains both sync tests and Lwt tests will first run
> all sync tests, then all Lwt tests.

### "focus"'ed tests

When working on a specific feature, you can add a `focus` call before a group
or a test to only run those tests as part of the run, yielding a faster feedback
or eliminating noise.

```ocaml
focus root_context "Scope" (fun s -> (* ... *) )

root_context "Scope" (fun s -> 
  focus s.context "Child scope" (fun s -> (* ... *) )
)

root_context "Scope" (fun s ->
  focus s.test "Test" (fun _ -> ())
)
```

> [!WARNING]
> Committing focus'ed tests to CI can lead to bugs appear in code that was
> otherwise covered by tests (which I have actually experienced). To prevent 
> that, a test tool should support the option of failing in the precense of
> focused tests for CI builds. Speed does not yet support this yet (neither did
> the tool we used back then despite that should have been a much more mature 
> tool).

## Root suite

The `root_context` call is the only _mutating_ line of code in Speed. It parses
the inner function and adds the generated suite to a special `root stuite`. You
can instead use a function `parse`, that returns the generated suite, letting
yourself build the root suite.

> [!NOTE]
> Synchronous test code, and Lwt test code are actually kept in two different
> root suites, but this is made transparent to the user.

## Running tests and test discovery

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

The concept is based on _an expectation_. A expectaion receives an _actual_
value and verifies that the value meets the expectation. Currently, the matcher
has the format `'a -> ('b, 'c) result`, but the will likely change into a record
containing more information, such as a descriptive neme.

Verifying an actual value against an expection is done using either `expect` or
`should`, the only difference between the two is the order in which the
arguments are passed.

```ocaml
(* Running with expect syntax. An optional name can be added *)
expect "foo" @@ equal_string "foo"
expect ~name:"First assertion" "foo" @@ equal_string "foo"
(* or *)
expect "foo" (equal_string "foo")

(* Running with should syntax. An optional name can be added *)
"foo" |> should @@ equal_string "foo"
"foo" |> should ~name:"First assertion" "foo" @@ equal_string "foo"
```

### Composing expectations

The expectation can return a new value in the result, which can be piped to a
new expectation. For example, the matchers for `result` values are defined here:

```OCaml
let be_ok = function
  | Ok x -> match_success x
  | Error _ -> match_failure ()

let be_error = function
  | Error x -> match_success x
  | Ok _ -> match_failure ()
```

### Writing Expectations for Record Objects.

When testing code that returns a record, you may be tempted to use an equality
function in test to compare an _actual_ value against an _expected_ value. This
approach does have some problems in many cases (this depends on the test).

When using TDD, each test should describe one _behaviour_ of the system. When
you add new behvaiour, existing tests should remain unchanged. And when you
change code relaing to _one_ behaviour, only tests relating to that behaviour
should change.

In order to support this, tests should be written so only data _relevant to the
outcome of the test_ is specified in the test; both when setting up the initial
state, and in the verification of the actual data.

This means that for record types, you should in most cases avoid using an
`equals` function, as the test code no longer compiles. But if you now add the
new actual value to the expectation, your test now specifies something which
wasn't relevant to the original _behaviour_ the test was supposed to verify.

Changes in the _new behaviour_ will cause the _old test_ to fail when it
shouldn't.

These are the types of experiences that makes people give up on TDD, a lot of
maintenance on test code, when the behaviour that should have been tested
actually didn't change.

For record types, one way is to verify each field explicitly, but Speed contains
a ppx deriver (consider it a type of code generator based on a type), that can
help write expectations for parts of a record value.

This can be combines with ppx_import to keep test code away from your production
code.

```ocaml
(* lib/domain.ml *)
type account = {
  id: account_id;
  email: string;
  first_name: string;
  last_name: string;
  password_hash: string;
}

(* test/domain_test.ml *)
type account = [%import: Lib.Domain.account] [@@deriving matcher]

test "New account has right email" (fun _ ->
  let account = make_account 
    ~email:"jd@example.com" 
    ~password:"1234" (* ... *)
    () in
  account |> should @@ match_account ~email:(equal_string "jd@example.coM")
)

test "New account has a hashed password" (fun _ ->
  let account = make_account 
    ~email:"jd@example.com" 
    ~password:"1234" (* ... *)
    () in
  account |> should @@ match_account ~password_hash:(match_password "1234");

  (* Note: This is for this example only of what you _might_ want to verify. 
     The `not` function does not yet exist in the system *)
  account.password_hash |> should @@ (not (equal_string "1234"));
)

test "New account has the right name" (fun _ ->
  let account = make_account ~email:"jd@example.com" ~password:"1234" () in
  account |> should @@ match_account ~password_hash:(match_password "jd@example.coM")
)
```

> [!NOTE]
> The previous code example does not follow the rules I just mentioned. Setup
> code specifies properties irrelevant to the actual behaviour tested. This 
> should be accomplished through "factory functions" in test code. I did not
> include that in the example as that's not something the test library supports,
> not at the moment at least - that is so far your own responsibility. I am
> unsure if you _can_ make a meaningful ppx rewriter, as it would be difficult
> to guarantee that the objects are valid from the perspective of the domain
> rules in the system.

#### Configuring the Rewriter When Using Dune

To use this, together with `ppx_import`, you need to use `staged_pps` instead 
of `pps` in the preprocess specification.

```
(test
  (libraries speed)
  (preprocess (staged_pps ppx_import speed.ppx_matcher))
  (name my_test_library))
```

## Test "subject" and Metadata

Speed supports having general "setup" code for a group of tests. The setup code
_may_ return a value, which will be accessible to tests in this group, as well
as child groups.

There are two different functions
 - `s.fixture` creates a _named_ child group with a setup function
 - `s.setup` creates an _anonymous_ child group (i.e. internally there is a 
   child, but that is transparent from the user - the tests appear is if part of
   the same suite.

I dislike syntax for `s.fixture`, so it _may_ break in the future.

```ocaml
root_context "Foo" (fun s ->
)
```

Nested setups get access to the value from parent setup, and can extend or
replace it.

> [!TIP]
> Using OCaml objects can make the test easier to read, if you need to return
> multiple values. This makes the values accessible under a friendly name; yet
> you don't have to first define a type as you would need for a record

```ocaml
root_context "Account creation" (fun s ->
  s.setup
    (fun _ -> 
      (* Imagine the output of the function is a record with the values, `entity`
         containins the domain object, and `events`, containin a list of domain
         events to publish *)
      let result = create_account (* ... *) ()
      object
        method account = result.entity
        method events = result.events
      end
    )
    (fun s ->
      s.test "Has the right account" (fun input ->
        input.subject#account |> should @@ match_account ~email:(* ... *) ()
      );

      s.test "Should generate a _created event_" (fun input ->
        let match_event = match_created_event (* ... *) ();
        input.subject#events |> should @@ contain_element (match_event)
      )
    )
  )
```

The subject available is only the output of the most direct setup function, so
if a value created in a parent setup is required after a child setup, be sure to
include the value in the new outcome.

For example:

```ocaml
root_context "Account creation" (fun s ->
  s.setup
    (fun _ -> 
      let result = create_account (* ... *) ()
      object
        method account = result.entity
        method events = result.events
      end
    )
    (fun s ->
      (* ... previous tests *)
      s.context "Account activation" (fun s ->
        s.setup (fun { subject; _ } ->
          let activation_event = get_activation_event subject#events in
          object
            method account = subject#account
            method activation_code = activation_event.activation_code
          end
        )
        (fun s ->
          s.test "Should succeed with the _right_ code" (fun x ->
            let sub = x.subject in
            let result = sub#account |> Account.activate sub#activation_code in
            result |> should (be_ok >=> match_account (* ... *))
          );

          s.test "Should fail when using the _wrong_ code" (fun x ->
            (* ... *)
          )
        )
      )
    )
  )

### Test metadata.

The _intention_ of the "setup" is to describe some general feature; but child
contexts can describe variations in which the feature operate. Each child suite
or test can contain _metadata_ which is accessible when the setup function is
executed for that test alone.

Each type of metadata needs to be created as a type extending the extensible
variant, `Speed.metadata`.

Metadata is added as a list to each context or test using the optional 
`?metadata` argument.

All metadata is available as a `metadata` field, both to setup functions, and
test functions. The field is of type `Speed.metadata list`. 

Metadata is added to the beginning of the list, as you descent into child
groups, meaning that in the case that the same type of metadata is specified
multiple times in the stack, the _closest_ value will be first in the list, when
searching from the beginning.

Consuming the metadata is a bit awkward, but there's another ppx rewriter so
simplify the code. First, without the rewriter:

```ocaml
(* Add the type to Speed.metadata *)
type Speed.metadata += IntValue of int
;;

root_suite "Metadata test" (fun s ->
  s.setup (fun x ->
    x.metadata 
    |> List.find_map function
      | IntValue x -> Some x
      | _ -> None
  )
  test "when value is 42" ~metadata:[IntValue 42] (fun {subject;_} ->
    subject |> should (be_some (equal_int 42))
  );
```

If you add `speed.ppx_metadata` to the list of pps preprocessors, you get the
ability to retrieve the value much more easily, and optionally specifying a
default value to use if no metadata exists.

```ocaml
root_suite "Metadata test" (fun s ->
  s.setup (fun test_input ->
    (* First, without the use of a default value, [%m ] creates a function that
       takes a metadata list as input, and returns an `'a option`.
    let optional_value_1 = [%m IntValue] test_input.metadata in
    (* [%mx takes the entire test_input as input *)
    let optional_value_2 = [%m IntValue] test_input in

    (* With the use of a fallback value when the data is not present *)
    let explicit_value_1 = [%m IntValue 42] test_input.metadata in
    let explicit_value_2 = [%mx IntValue 42] test_input in
    (* ... *)
  )
```

### Motivating Examples
Th

```ocaml
type Speed.Domain.metadata += Body of string;;

root_context "POST /registration" (fun s ->
  s.setup
    (fun x ->
      let body = x |> [%mx Body ""] in
      handle_result @@
        post "/registration"
        ~headers:["content-type", "application/x-www-form-urlencoded"]
        ~body ()
    )
    (fun s ->
      s.context ~metadata:[Body "email=john.doe%40example.com"] "Email is valid" 
        (fun s ->
          s.test "Should generate a status 200" (fun { subject; _ } ->
            subject |> should @@ have_status 200
          );

          s.test "Should return a 'please check your email' reply" (fun { subject; _ } ->
            let+ body = subject >>= Dream.body in
            body |> should @@ contain "Please check your email"
          )
        );

      s.context ~metadata:[Body "email=jane.doe%40example.com"] "Email is a duplicate"
        (fun s ->
           s.test "Should generate a status 200" (fun { subject; _ } ->
             let+ status = subject >|= Dream.status >|= Dream.status_to_int in
             status |> should @@ equal_int 200
           );

           s.test "Should return a 'please check your email' reply" (fun { subject; _ } ->
             let+ body = subject >>= Dream.body in
             body |> should @@ contain "duplicate"
           )
        );

      s.context ~metadata:[Body "email=invalid"] "Email is not a valid email" 
        (fun s ->
          (* ... *)
        );

      s.context ~metadata:[Body "bad_name=jd%40example.com"] "Body does not contain an email" 
        (fun s ->
          (* ... *)
        )
    )
)
```

I would like to find a better DSL for specifying metadata. An alternate version
exists with the `with_metadata` helper, that allows you to place the metadata
before the test/context, but currently ocamlformat brings everything together on
one line, not giving the separation I'd like.

```ocaml
with_metadata [IntValue 1] s.fixture fun s ->
  with_metadata [IntValue 2] s.test "Test" (fun { subject; _ } ->
    (* ... *)
  )
)
```
