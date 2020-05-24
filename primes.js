// for benchmarking against the spruce equivalent

function isPrime(n) {
    var prime = true;
    for(i = 2; i < n; i++) {
        prime = prime && (!isFactor(n, i));
    }
    return prime;
}

function isFactor(x, y) {
    return x % y == 0
}

function main() {
    primes = [];
    for(i = 3; i < 10000; i++) {
        if(isPrime(i)) {
            primes.push(i);
        }
    }
    console.log(primes);
}

main();
