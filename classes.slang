class K1(x: Int) {
    def foo(): K1 =
        return K1(123)
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
k2.k.foo() = k1.foo()
print(k2)