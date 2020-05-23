type Maybe(a) {
    Just(a)
    Nothing
}

map(m, fn) {
    case m {
        Just(x) -> Just(fn(x))
        Nothing -> Nothing
    }
}

foo(m) {
    case m {
        Just(m2) -> {
            case m2 {
                Just(x) -> Just(x)
                Nothing -> Nothing
            }
        }
        Nothing -> Nothing
    }
}
