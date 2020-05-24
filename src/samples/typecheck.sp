type Maybe(a) {
    Just(a)
    Nothing
}

type List(a) {
    Cons(a, List(a))
    Nil
}

type CoolType(a) {
    Big(List(a))
    Small(Maybe(a))
}

coolFn(cool) {
    case cool {
        Big(l) -> {
            case l {
                Cons(x, r) -> Just(x)
                Nil -> Nothing
            }
        }
        Small(m) -> m
    }
}
