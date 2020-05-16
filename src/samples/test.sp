mut myGlob = 0

addGlob(n) {
    myGlob := myGlob + n
    myGlob
}

type Bool {
    True
    False
}

not(b) {
    case b {
        True -> False
        False -> True
    }
}

and(x, y) {
    case x {
        True -> {
            case y {
                True -> True
                False -> False
            }
        }
        False -> False
    }
}

type Maybe {
    Just(Float)
    Nothing
}

map(maybe, fn) {
    case maybe {
        Just(n) -> Just(fn(n))
        Nothing -> Nothing
    }
}

inc(x) {
    x + 1
}

main() {
    addGlob(10)

    mut myVal = Just(5.0)
    myVal := map(myVal, inc)
}
