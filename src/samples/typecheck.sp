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

bar(m, fn) {
    case True {
        True -> Just(fn(m))
        False -> Nothing
    }
}

not(b) {
    case b {
        True -> False
        False -> True
    }
}

foo(x, y, fn) {
    z1 = x + 1
    z2 = not(y)
    fn(id(x), id(y))
}
