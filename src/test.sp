x = 0

addGlob(n) {
    x := x + n
    x
}

fib(n) {
    case n {
        True -> n
        False -> fib(n-1) + fib(n-2)
    }
}

type Foo {
    Bar(Int)
    Baz
}

foo(item) {
    mut a = 0
    case item {
        Bar(x) -> {
            a = x
        }
        Baz -> {
            a = 10
        }
    }

    a
}

main() {
    addGlob(10)
}
