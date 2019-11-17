function fib(n) {
    if (n < 2) {
        return n;
    }
    let a = fib(n - 1);
    return a + fib(n - 2);
}
fib(16)
