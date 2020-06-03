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
* Algebraic Data Types (ADTs): ADTs have been kicking around in languages like
  Haskell for decades, but only recently have they found their way into the
  mainstream, with appearances in the likes of Rust and Typescript. They're
  awesome, and I think every language should feature them
* Pattern Matching: sort of a sibling feature to ADTs, these allow programmers
  to destrucure ADTs and use the contents
* List Comprehensions: Python-style list comprehensions are great tools for
  readably building lists
* JS as a Compile Target: a number of languages, for instance Elm and
  ReasonML, use JS as their primary compile target, which these days is 
  actually very pragmatic. JS runs everywhere: browsers, servers, desktop
  applications, phones, plus it allows the language to leverage highly
  optimized JS runtimes (A.K.A. I'm lazy and don't want to write the Garbage
  Collector)
* Friendly and Helpful Compiler: Elm's compiler has the most beginner-friendly
  error messages I've ever seen. Good compiler messages can save trips to stack
  overflow, which is a huge productivity (and quality of life) improvement.

Those are the respectable ideas. For the wholly _unrespectable_ ideas,
[have a look in here](WackyIdeas.md).

### What does it look like?

The following program computes primes below 1000:

```
filter(ls: List(a), fn: (a) -> Bool) -> List(a) {
    case ls {
        Cons(rest, v) -> {
            case fn(v) {
                True -> Cons(filter(rest, fn), v)
                False -> filter(rest, fn)
            }
        }
        Nil -> Nil
    }
}

range(start: Int, end: Int) -> List(Int) {
    case start < end {
        True -> Cons(range(start + 1, end), start)
        False -> Nil
    }
}

isPrime(n: Int) -> Bool {
    checkFactors = range(2, n)
    factors = filter(checkFactors, isFactor@(n, _))

    case factors {
        Cons(rest, val) -> False
        Nil -> True
    }
}

isFactor(x, y) {
    x % y == 0
}

main() {
    nums = range(3, 1000)
    filter(nums, isPrime)
}
```

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
