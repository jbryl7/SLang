var x: Int = 10
def fibonacci(n: Int): Int = {
    var x: Int = 15
    if (n < 2) {
         return 1
        } else
        return fibonacci(n-1) + fibonacci(n-2)
}
var y: Int = 5

def addOne(): Unit =
    x = x + 1
def addOneButNotX(x: Int): Unit =
    x = x+1

print("x = "+ x)
addOne()
addOneButNotX(y)
print("fib(7): " + fibonacci(7))
print("fib(x): " + fibonacci(x))
print("x = " + x)
