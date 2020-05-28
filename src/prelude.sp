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

type Maybe(a) {
    Just(a)
    Nothing
}

andThen(m, fn) {
    case m {
        Just(val) -> fn(val)
        Nothing   -> Nothing
    }
}

type List(a) {
    Cons(List(a), a)
    Nil
}

listMap(ls, fn) {
    case ls {
        Cons(rest, val) -> Cons(listMap(rest, fn), val)
        Nil -> Nil
    }
}
