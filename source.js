function fib(n) {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

let i = 0;
while (true) {
    if (fib(i) > 10) {
        break;
    }
    i = i + 1;
}
i
