type Maybe {
    Just(Int)
    Nothing
}

map(m, fn) {
    case m {
        Just(x) -> fn(x)
        Nothing -> 0
    }
}

