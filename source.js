function fib(n) {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

let i = 0;
let result = undefined;
while (true) {
    result = fib(i);

    if (++i >= 30) {
        break;
    }
}
result
