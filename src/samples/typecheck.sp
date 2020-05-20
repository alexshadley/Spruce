num(n) {
    case n == 0 {
        True  -> 0
        False -> num(n - 1) + 1
    }
}

main() {
    num(5)
}
