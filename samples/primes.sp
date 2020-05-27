filter(ls, fn) {
    case ls {
        Cons(v, rest) -> {
            case fn(v) {
                True -> Cons(v, filter(rest, fn))
                False -> filter(rest, fn)
            }
        }
        Nil -> Nil
    }
}

filter2(ls, fn, firstArg) {
    case ls {
        Cons(v, rest) -> {
            case fn(firstArg, v) {
                True -> Cons(v, filter2(rest, fn, firstArg))
                False -> filter2(rest, fn, firstArg)
            }
        }
        Nil -> Nil
    }
}

range(start, end) {
    case start < end {
        True -> Cons(start, range(start + 1, end))
        False -> Nil
    }
}

isPrime(n) {
    checkFactors = range(2, n)
    factors = filter2(checkFactors, isFactor, n)

    case factors {
        Cons(val, rest) -> False
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
