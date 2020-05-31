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
    }




var lst: List = List(4, List(3,List(4,List(5,nil))))
print(lst.add(4))
print(lst.get(3))