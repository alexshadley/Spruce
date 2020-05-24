type Maybe(a) {
    Just(a)
    Nothing
}

flatten(m) {
    case m {
        Just(m2) -> m2
        Nothing -> Nothing
    }
}

withDefault(m, d) {
    case m {
        Just(v) -> v
        Nothing -> d
    }
}

type List(a) {
    Cons(a, List(a))
    Nil
}

range(start, end) {
    case start < end {
        True -> Cons(start, range(start + 1, end))
        False -> Nil
    }
}

foldl(fn, start, list) {
    case list {
        Cons(x, rest) -> fn(x, foldl(fn, start, rest))
        Nil -> start
    }
}

add(x, y) {
    x + y
}

type Map(a, b) {
    MapInner(List(Tuple(a, b)))
}

type Tuple(a, b) {
    Tup(a, b)
}

empty() {
    ret = MapInner(Nil)
    ret
}

get(map, k) {
    case map {
        MapInner(ls) -> {
            case ls {
                Cons(t, rest) -> {
                    case t {
                        Tup(key, val) -> {
                            case k == key {
                                True -> Just(val)
                                False -> get(MapInner(rest), k)
                            }
                        }
                    }
                }
                Nil -> Nothing
            }
        }
    }
}

insert(map, k, v) {
    case map {
        MapInner(ls) -> MapInner(Cons(Tup(k, v), ls))
    }
}

main() {
    myList = range(0, 10)
    myVal = foldl(add, 0, myList)

    mut dict = empty()
    dict := insert(dict, 9, Just(3))
    dict := insert(dict, 4, Just(2))
    dict := insert(dict, 5, Nothing)

    flatten(get(dict, 9))
}
