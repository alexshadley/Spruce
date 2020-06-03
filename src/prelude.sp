interop console.log(x: Int) -> ()

type Bool {
    True
    False
}

not(b: Bool) -> Bool {
    case b {
        True -> False
        False -> True
    }
}

type Maybe(a) {
    Just(a)
    Nothing
}

andThen(m: Maybe(a), fn: (a) -> Maybe(b)) -> Maybe(b) {
    case m {
        Just(val) -> fn(val)
        Nothing   -> Nothing
    }
}

type List(a) {
    Cons(List(a), a)
    Nil
}

listMap(ls: List(a), fn: (a) -> b) -> List(b) {
    case ls {
        Cons(rest, val) -> Cons(listMap(rest, fn), fn(val))
        Nil -> Nil
    }
}
