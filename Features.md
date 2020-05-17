# Planned and Existing Features

Spruce currently compiles, but with a reduced feature set. This document
exists to coordinate the work of building new features.

## Primitives

|Type|Status|
|----|------|
| Bool | |
| Int | |
| Float| :heavy_check_mark: |
| Char | |
| String | |
| Tuple | |

In general the Spruce approach to primitives has not revieved a lot of
thought. My general instinct is to implement as many types as possible in
Spruce's type system, but obviously that doesn't make sense for things like
Int and Float. Also, I don't think String should be implemented as a List of
Char, as is done in Haskell.

## Lists

| Feature | Status |
|---------|--------|
| Lists | |
| List Indexing (Python-style) | |
| List Comprehension | |
| List Iteration (for loop) | |

Technically, Lists can currently be constructed in Spruce with ADTs as such:

```
type List {
    Cons(Float, List)
    Nil
}
```

This is the list definition one might expect to see in functional languages.
However, for performance reasons it probably makes more sense to back Spruce's
lists with JavaScript lists. Ideally interfaces will make it possible for both
versions of a list to be used in things like for loops. One of the main design
goals of Spruce is to make list processing easy and expressive with a mix of
functional and imperative concepts.

## Type Checking

| Feature | Status |
|---------|--------|
| Type Annotations | |
| Type Inference | |
| Typeclasses | |

The type checker is maybe the most important feature Spruce currently lacks.
The eventual goal is to adapt the Hindley-Milner type inference algorithm to
Spruce, which would allow programmers to optionally specify function and
variable types, as well as enabling generics. This may come later though, once
more of the language's semantics are understood.

## ADTs

| Feature | Status |
|---------|--------|
| ADT | :heavy_check_mark: |
| Type Parameters | |

ADTs are implemented, but not with type parameters, so essentially ADTs are
restricted to one type. Without a type checker that's not really a
restriction, but in the future this will be an issue.

## Pattern Matching

| Feature | Status |
|---------|--------|
| ADT matching | :heavy_check_mark: |
| Any (underscore) | |
| Numeric patterns | |
| String patterns | |
| Tuples | |

Pattern matching currently works to allow destructuring of ADT values, but
cannot do anything useful with numeric types, or strings or tuples when those
are implemented.

## Parser

| Feature | Status |
|---------|--------|
| Basic language parsing | :heavy_check_mark: |
| Multiline Statements | |
| Understanding of Indentation | |

The current parser can take language into an AST, but it lacks any
understanding of indentation, and as such can't currently parse multiline
statements.

## Codegen

| Feature | Status |
|---------|--------|
| Code Generation | :heavy_check_mark: |
| Indentation | |
