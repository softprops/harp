package harp

import scala.concurrent.duration.{ Duration => ScalaDuration }

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

  case class Num(
    value: Int, comments: Option[String] = None) extends Value {
    type Self = Num
    def withComments(cs: Option[String]) = copy(comments = cs)
  }

  case class Duration(
    value: ScalaDuration, comments: Option[String] = None) extends Value {
    type Self = Duration
    def withComments(cs: Option[String]) = copy(comments = cs)
  }
}
