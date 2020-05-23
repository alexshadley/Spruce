type Maybe(a) {
    Just(a)
    Nothing
}

map(fn, m) {
    case m {
        Just(x) -> Just(fn(x))
        Nothing -> Nothing
    }
}

main() {
    a = 1
}
