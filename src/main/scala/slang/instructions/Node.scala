package slang.instructions

trait Node {
  def getNest(nested: Int): String = {
    var nest = ""
    for (x <- 0 to nested)
      nest += "  "
    nest
  }
}
