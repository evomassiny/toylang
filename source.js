function fib(n) {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

let result = undefined;
for (let i = 0; i < 30; i++) {
    result = fib(i);
}
result
