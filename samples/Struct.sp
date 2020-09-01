inc(x) {
    x + 1
}

coordinify(x, y) {
    [x = x, y = y]
}

main() {
    x = 1
    y = 2

    val = [ a = x + y, b = inc@(x), c = True ]
    val
}
