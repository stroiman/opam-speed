# Speed - OCaml test runner

Feeling brave, and want to try this out, [read the documentation](./DOCUMENTATION.MD)

This test tool was build as I was unhappy with the current options of testing
tools; none of them seem to match my personal philosophy.

It seems that they have a lot of focus on "testing", which may seem obvious, but
that is far from what a testing tool is for me. When focusing on testing you
are typically _adding tests_ to existing code, often guided by where you most
fear things might break in the future.

This is nowhere near how I work. For me, a testing tool is merely a series of
microtasks executed everytime I save a file; the goal being providing fast
feedback to my work. When I write code, I save often, and every time I save, I
want feedback; does the code work as expected.

This ultra short feedback loop; combined with the ability to refactor, increases
my productivity by huge amounts.

The fact that I have tests to verify behaviour later on is a secondary BUT STILL
important side effect.

But the test tool serves an active role in me working with the code; and I did
not find that the existing options supported this rolw very well.

> [!NOTE]
> Looking at the origin of the git history, the initial commit is creating a
> skeleton dune project. **The second commit** sets up a feedback loop. The
> feedback loop is initially just writing to std-out. This is fine for this
> purpose as the goal right now is feedback; not a CI-ready verification tool.

> [!NOTE] This tool became self-testing from the 6th commit; a new personal
> record (previous record was 11th commit). And that is without squashing
> commits to artificially work towards this; but I was focusing specifically on
> this goal.

## The name?

Well, before writing this, I did use Alcotest for a short while; and inspired by
that, the word "amphetamine" popped into mind (which is incidentally also an
ingredient in some ADHD medicine).

"Speed" is slang for aphetamine, and I found that to be very fitting; as the
_primary goal_ of a test tool it to _increase my speed_.

## Philosophy

The testing tool should in my opinion be a microtask runner. Each "test" is a
small task that is executed whenever you save a file. And I prefer the term
specification; as I start by writing this, expressing what my code is _intended_
to do.

Second; it should be a tool that help setup a fixture, so there is a focus on
_verifying_ a that some _specification_ of a _feature_ works correctly in a
given _fixture_.

Fixtures can be refined, e.g. a fixture could represent a user existing in the
system already with a given password. A refined feature could be that there have
already been made two bad login attempt (for verifying account lock out
behaviour)

When working on a specific feature, you can "focus" the relevant specifications;
providing the relevant feedback as quickly as possible without unnecessary
noise;

> [!NOTE]
> A tool should also be able to fail the build on CI servers in the presence of
> "focused" tests. I did once experience a bug for a feature that was in fact
> covered by a test; But this was not caught as code had been committed with
> focused tests (the test framework did not support this behaviour)

### Assertion framework is out of scope (but currently included)

It is _not_ in scope to include an assertion framework. The goal of this is to
create a super effective microtask runner. Other tools may create the perfect
assertion framework.

But I also want to have the ability to have assertions, so right now; this
project is the nesting ground for an assertion framework; and there is _some_
dependency from the test runner to the assertion code in order to format errors
nicely.

It is also an area I have tried before, and never felt I found the right design.
Chai stands out to me as the best assertion framework; however it fully embraces
the dynamic nature of JavaScript; which of course doesn't translate to OCaml at
all. If it ends out being a good assertion framework; it will probably make its
way into a package of its own.

## Design goals

It is the goal that this should be highly extensible. It is also a goal that how
things are extended can be mixed between different parts of the tests. E.g. pure
code can be tested without the need for async libraries like Lwt, whereas
tests for database, network, or other IO code can be written natively using
these libraries.

It is also a clear design goal to utilise OCaml5 multicore support.

The system should be highly extensible. As an example. by default, a
specification is a `unit -> unit` function, and a failed test will raise an
exception. But for testing IO code; the type would be `unit -> unit promise`.
And if you'd rather want the test to return a `result` instead of raising an
exception; that should also be possible.

The _outcome_ of running a test suite is passed to a _reporter_. If you define
your own outcome of a test; you must define your own _reporter_ to handle the
outcome; such that the outcome will be handled correctly.

There is a simple `run_main` function, that your own code can use to run the
test suite. It basically does this

```
let run_main suite =
  match is_success @@ run_suite suite with true -> exit 0 | false -> exit 1
```

There's a bit more to it right now; but that's the basic idea.

This funct btw, is the only piece of code that is not explicitly tested; However
it is implicitly tested, as I use it in my own test suite; and I always start
with a failing test.

## Previous experiences

I have previously created a FSpec, a testing tool for F#; and Respect, a
testing tool writting in ReasonML (but with focus on the JavaScript side of
things).

I made a lot of good solutions in FSpec; but I did make the mistake of initially
applying the same solutions to Respect; not recognising the differences between
running in .NET CLR, and OCaml. Also, I started on this while my OCaml
experience was still rather low, with little knowledge of the unique features of
OCaml, such as the powerful module system, or polymorphic variants.
