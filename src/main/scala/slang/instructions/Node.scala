package slang.instructions

trait Node {
  def getNest(nested: Int): String = {
    var nest = ""
    for (x <- 0 to nested)
      nest += "  "
    nest
  }
  def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest} Node"
  }
}
