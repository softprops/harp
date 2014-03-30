package harp

sealed trait Value {
  type Self <: Value
  def comments: Option[String]
  def withComments(cs: Option[String]): Self
}
object Value {
  case class Str(
    value: String, comments: Option[String] = None) extends Value {
    type Self = Str
    def withComments(cs: Option[String]) = copy(comments = cs)
  }
}
