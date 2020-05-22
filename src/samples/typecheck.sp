type Maybe(a) {
    Just(a)
    Nothing
}

andThen(fn, m) {
    case m {
        Just(x) -> fn(x)
        Nothing -> Nothing
    }
}

id(x) {
    x
}

main() {
    i = id(Just(5))
    b = id(Just(True))
}
