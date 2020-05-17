mut myGlob = 0

addGlob(n) {
    myGlob := myGlob + n
    myGlob
}

type Bool {
    True
    False
}

not(b) {
    case b {
        True -> False
        False -> True
    }
}

and(x, y) {
    case x {
        True -> {
            case y {
                True -> True
                False -> False
            }
        }
        False -> False
    }
}

type Maybe {
    Just(Float)
    Nothing
}

map(maybe, fn) {
    case maybe {
        Just(n) -> Just(fn(n))
        Nothing -> Nothing
    }
}

inc(x) {
    x + 1
}

type List {
    Cons(Float, List)
    Nil
}

listMap(ls, fn) {
    case ls {
        Cons(val, l) -> Cons(fn(val), listMap(l, fn))
        Nil          -> Nil
    }
}

listReduce(fn, start, ls) {
    case ls {
        Cons(val, l) -> fn(val, listReduce(fn, start, l))
        Nil -> start
    }
}

add(a, b) {
    a + b
}

fib(n) {
    case n > 1 {
        True  -> fib(n-1) + fib(n-2)
        False -> n
    }
}

main() {
    addGlob(10)

    myBool = and(True, False)

    mut myVal = Just(5.0)
    myVal := map(myVal, inc)

    mut myList = Cons(1.0, Cons(2.0, Cons(3.0, Nil)))
    myList := listMap(myList, fib)
    listReduce(add, 0, myList)
}
