class List(head: Int, tail: List) {
    def get(x: Int): Int = {
        if (x <= 0)
            return this.head
        if (this.tail != nil)
            return this.tail.get(x-1)
        else
            return nil
        }

    def add(x: Int): List =
        return List(x, this)

    def length(): Int = {
        if (this.tail == nil) return 1
        else return 1 + this.tail.length()
    }
}

var lst: List = List(4, List(3,List(4,List(5,nil))))

print(lst.add(4))
print("lst = " + lst)
print("lst at index(3) = "+ lst.get(3))
print("length = " + lst.length())
var x: Int = 24
 x = 42
 print(x)