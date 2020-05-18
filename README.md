# Spruce: it's kinda fun

Spruce is a language experiment aimed at collecting the best language
innovations of the past decade or so into an awesome swiss-army knife of a
tool.

### I'm impatient, give me a TL:DR;

Spruce is Rust if Rust were garbage collected and less picky.

From an alternative perspective, Spruce is Python if Python were statically
typed and had ADTs.

### Language innovations of the last decade? Like what?

Some of the things I want to see in Spruce:

* Type Inference: Advanced type inference (think Haskell here) lets us have
  the awesome guarantees of a static type system without bogging ourselves
  down in type annotations everywhere.
* Albebraic Data Types (ADTs): ADTs have been kicking around in languages like
  Haskell for decades, but only recently have they found their way into the
  mainstream, with appearances in the likes of Rust and Typescript. They're
  awesome, and I think every language should feature them
* Pattern Matching: sort of a sibling feature to ADTs, these allow programmers
  to destrucure ADTs and use the contents
* List Comprehensions: List comprehensions are great tools for readably
  building lists
* JS as a Compile Target: a number of languages, for instance Elm and
  ReasonML, use JS as their primary compile target, which these days is 
  actually very pragmatic. JS runs everywhere: browsers, servers, desktop
  applications, phones, plus it allows the language to leverage highly
  optimized JS runtimes (A.K.A. I'm lazy and don't want to write the Garbage
  Collector)

### Why?

I'm a bored college graduate with time on his hands and a slight background in
programming languages from research and coursework. It's mostly fun for me,
but I do believe a tool like this doesn't exist yet and totally should.

### Is it ready for me to use?

Spruce does technically compile! However, we are currently compiling on a
heavily reduced feature set that would make real programs difficult to
develop. Additionally, the language's syntax and semantics are prone to change
at any point, so Spruce is not yet suited for real production applications.

And that's not even to mention library support!

[Track the progress of these features here](Features.md)

### Do you accept PRs?

Absolutely! I'm a very amateur Rust programmer, so I would greatly appriceate
the input from others more experienced (and heck, if you're an amateur too
then we'll struggle together).
