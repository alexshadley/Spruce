filter(ls, fn) {
    case ls {
        Cons(rest, v) -> {
            case fn(v) {
                True -> Cons(filter(rest, fn), v)
                False -> filter(rest, fn)
            }
        }
        Nil -> Nil
    }
}

filter2(ls, fn, firstArg) {
    case ls {
        Cons(rest, v) -> {
            case fn(firstArg, v) {
                True -> Cons(filter2(rest, fn, firstArg), v)
                False -> filter2(rest, fn, firstArg)
            }
        }
        Nil -> Nil
    }
}

range(start, end) {
    case start < end {
        True -> Cons(range(start + 1, end), start)
        False -> Nil
    }
}

isPrime(n) {
    checkFactors = range(2, n)
    factors = filter2(checkFactors, isFactor, n)

    case factors {
        Cons(rest, val) -> False
        Nil -> True
    }
}

isFactor(x, y) {
    x % y == 0
}

main() {
    nums = range(3, 1000)
    filter(nums, isPrime)
}
