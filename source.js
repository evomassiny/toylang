function foo(b, t) {
    if (b) {
        return b;
    }
    return true;
}

let a = 22;
let c = foo(a, "test");

let flag = 100;
while (flag) {
    flag = flag - 1;
}
flag
