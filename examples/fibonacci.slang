def fibonacci(n: Int): Int = {
    if (n < 2) {
        return 1
        } else
        return fibonacci(n-1) + fibonacci(n-2)
}

var x: Int = 7
print("fib(7): " + fibonacci(x))
print("fib(12): " + fibonacci(12))