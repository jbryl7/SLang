class K1(x: Int) {
    def foo(x: Int): Int =
        return this.x + x
}
class K2(k: K1) {}
var k1: K1 = K1(10)
var k2: K2 = K2(k1)
var k11: K1 = K1(15)
print(k2)
k2.k.x = 42
print(k2)
k2.k = k11
print(k2)
print(k1.foo(3))