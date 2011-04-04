var fib = function(x) {
    if (x === 0) {
        return 1;
    } else if (x === 1) {
        return 1;
    } else {
        return fib(x - 1) + fib(x - 2);
    }
};

var time = function(f) {
    var t = new Date()
    var a = f();
    console.log("Elapsed " + (new Date().getTime() - t.getTime()))
    return a;
};