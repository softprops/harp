package harp

sealed trait Section {
  def name: String
  def comments: Option[String]
  def options: Map[String, Value]
  def get = options.get(_)
}

object Section {
  abstract class Named(val name: String) extends Section
  case class Defaults(options: Map[String, Value], comments: Option[String]) extends Named("defaults") {
    def set(value: (String, Value)) = copy(options = options + value)
  }
  case class Global(options: Map[String, Value], comments: Option[String]) extends Named("global") {
    def set(value: (String, Value)) = copy(options = options + value)
  }
  case class Frontend(name: String, options: Map[String, Value], comments: Option[String]) extends Section {
    def set(value: (String, Value)) = copy(options = options + value)
  }
  case class Backend(name: String, options: Map[String, Value], comments: Option[String]) extends Section {
    def set(value: (String, Value)) = copy(options = options + value)
  }
  case class Server(name: String, options: Map[String, Value], comments: Option[String]) extends Section {
    def set(value: (String, Value)) = copy(options = options + value)
  }
  case class Listener(name: String, options: Map[String, Value], comments: Option[String]) extends Section {
    def set(value: (String, Value)) = copy(options = options + value)
  }
}
