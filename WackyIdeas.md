# Wacky Ideas

This document is for the wackier ideas we want to experiment with putting
into spruce. Feel free to make a PR and add your own ideas!

## Currying for c-syntax

Currying (also known as partial function application) in functional languages
traditionally goes something like this:

```
add :: Int -> Int -> Int
add x y = x + y
add3 = add 3
```

Essentially, we can specify the first argument, and get back another function
that adds 3 to whatever input we pass in. What's not immediately apparent is
that this isn't actualy a language feature, it's just a natural consequence of
the way functions are declared in languages like Haskell. The `->` operator is
right associative, so that type defintion acutally means
`Int -> (Int -> Int)`, or 'function from `Int` to function from `Int` to
`Int`. When we call the function with two arguments you're actually currying
and then immediately using your curried function. Neat, huh?

This is not only interesting PL theory, it's actually very useful any time
you're working with higher order functions. Most languages these days support
closures (i.e. functions that return functions), but not currying, which is a
shame because currying lets us very easily create new functions on the fly.
Certainly there's no theoretical reason we can't have closures in a language
like Spruce, so what gives? As far as I know, there is no precedent for what
this would look like syntactically, so what if we introduced some, ah, _spicy_
new syntax to make this feature reality? This little guy hasn't gotten much
love in the last decade: `@`, so let's give it a shot:

```
add(x: Int, y: Int) -> Int {
    x + y
}

add3 = add@(3, _)
```

Ok, so that's an interesting toy example. Makes enough sense. But let's see an
example where a real programmer might actually use this feature.

```
isFactor(x: Int, y: Int) -> Bool {
    x % y == 0
}

isPrime(n: Int) -> Bool {
    checkFactors = range(2, n)
    factors = filter(checkFactors, isFactor@(n, _))

    case factors {
        Cons(rest, val) -> False
        Nil -> True
    }
}
```

Aha! I knew I was writing this rant for a reason! The `filter` function needs
it's second argument, the condition function, to take one argument and return a
`Bool`, but `isFactor` takes two arguments.

Another neat consequence of doing currying this way is that we can specify the
arguments in any order we please, which isn't actually possible in languages
like Haskell, where currying has to go in the order of a function's arguments.
This forces programmers to think about how they're going to curry their
function as they define it. `map` takes a list and a function, but which comes
first depends on if programmers are going to want to make a function to map
a bunch of functions over the same list, or a function to map the same
function over a bunch of lists. Hard to know! Sure, there are workarounds
like the `swap` function that swaps the order of a function's arguments, but
I'd hardly call that an ergonomic solution.
