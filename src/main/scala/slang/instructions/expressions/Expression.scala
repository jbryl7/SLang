package slang.instructions.expressions
case class MyString(value: String) extends AnyVal
object MyString {
  implicit def myStringToString(myString: MyString): String = myString.value
  implicit def stringToMyString(string: String): MyString = MyString(string)
}
