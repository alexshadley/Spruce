type Maybe(a) {
    Just(a)
    Nothing
}

id(x) {
    x
}

map(m, fn) {
    case m {
        Just(x) -> Just(fn(x))
        Nothing -> Nothing
    }
}

andThen(fn, m) {
    case m {
        Just(x) -> fn(x)
        Nothing -> Nothing
    }
}

flatten(m) {
    case m {
        Just(m2) -> {
            m2
        }
        Nothing -> 0
    }
}

type Tuple(a, b) {
    Tup(a, b)
}

fst(t) {
    case t {
        Tup(x, y) -> x
    }
}

snd(t) {
    case t {
        Tup(x, y) -> y
    }
}
