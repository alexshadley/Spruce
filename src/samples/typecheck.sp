type Maybe(a) {
    Just(a)
    Nothing
}

id(x) {
    x
}

map() {
    mut x = Just(5)
    case x {
        Just(y) -> Just(id(y))
        Nothing -> Nothing
    }
}
