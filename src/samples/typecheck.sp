type Maybe(a) {
    Just(a)
    Nothing
}

id(x) {
    x
}

map(m, fn) {
    case m {
        Nothing -> Nothing
        Just(x) -> Just(fn(x))
    }
}
