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

function test(v) { 
    if (v == 21) {
        return true;
    } else {
        return false;
    }
}
test("21")
