interop console.log(x: a) -> ()

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

fold(ls: List(a), fn: (a, b) -> b, start: b) -> b {
    case ls {
        Cons(rest, val) -> {
            fn(val, fold(rest, fn, start))
        }
        Nil -> start
    }
}

type Tuple(a, b) {
    Tup(a, b)
}

type Dict(k, v) {
    DictInner(List(Tuple(k, v)))
}

dictEmpty() {
    val = DictInner(Nil)
    val
}

dictGet(dict: Dict(k, v), key: k) -> Maybe(v) {
    case dict {
        DictInner(ls) -> fold(ls, foldGetInner@(key, _, _), Nothing)
    }
}

foldGetInner(matchKey: k, tup: Tuple(k, v), maybeVal: Maybe(v)) -> Maybe(v) {
    case tup {
        Tup(key, val) -> {
            case key == matchKey {
                True  -> Just(val)
                False -> maybeVal
            }
        }
    }
}

dictSet(dict: Dict(k, v), key: k, val: v) -> Dict(k, v) {
    case dict {
        DictInner(ls) -> DictInner(Cons(ls, Tup(key, val)))
    }
}
