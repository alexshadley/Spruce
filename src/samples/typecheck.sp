type Maybe(a) {
    Just(a)
    Nothing
}

type List(a) {
    Cons(a, List(a))
    Nil
}


foo(f, g, h, v) {
    val = h(Just(g(f(v))))

    case val {
        Just(x) -> Just(x)
        Nothing -> Nothing
    }
}

map(ls, fn) {
    case ls {
        Cons(x, l) -> Cons(fn(x), map(l, fn))
        Nil -> Nil
    }
}

wrap(v) {
    ret = Cons(v, Nil)
    ret
}

main() {
    l1 = Cons(1, Cons(2, Cons(3, Nil)))
    l2 = map(l1, wrap)
}
