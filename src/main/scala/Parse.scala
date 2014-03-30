package harp

import scala.util.parsing.combinator.RegexParsers

class Parse extends RegexParsers {

  def ws: Parser[String] = """\s*""".r

  def id: Parser[String] = """[0-9A-Za-z-_.:]+""".r

  def comment: Parser[String] = """#.+""".r

  def config: Parser[Config] = section.* ^^ {
    case sections => Config(sections)
  }

  def section: Parser[Section] =
    global | defaults | frontend | backend | listener

  def comments: Parser[String] =
    comment.* ^^ {
      case lines =>
        lines.map(_.drop(1).dropWhile(_ == ' ')).mkString("\n")
    }

  def global: Parser[Section.Global] =
    comments.? ~ ("global" ~> options) ^^ {
      case comments ~ opts => Section.Global(opts, comments.filter(_.nonEmpty))
    }

  def defaults: Parser[Section.Defaults] =
    comments.? ~ ("defaults" ~> options) ^^ {
      case comments ~ opts => Section.Defaults(opts, comments.filter(_.nonEmpty))
    }

  def frontend: Parser[Section.Frontend] =
    comments.? ~ ("frontend" ~> name) ~ options ^^ {
      case (comments ~ name ~ opts) => Section.Frontend(name, opts, comments.filter(_.nonEmpty))
    }

  def backend: Parser[Section.Backend] =
    comments.? ~ ("backend" ~> name) ~ options ^^ {
      case comments ~ name ~ opts =>
        Section.Backend(name, opts, comments.filter(_.nonEmpty))
    }

  def listener: Parser[Section.Listener] =
    comments.? ~ ("listen" ~> name) ~ name.? ~ options ^^ {
      case comments ~ lname ~ addr ~ opts =>
        val hostport = addr.flatMap {
          _.split(":", 2) match {
            case Array(host, port) => Some(Map(
              "host" -> Value.Str(host),
              "port" -> Value.Str(port)
            ))
            case Array(port) => Some(Map(
              "port" -> Value.Str(port)
            ))
            case _ => None
          }
        }
        Section.Listener(
          lname,
          opts ++ hostport.getOrElse(Map.empty[String, Value]),
          comments.filter(_.nonEmpty))
    }

  //def server: Parser[Server] = */

  def options: Parser[Map[String, Value]] =
    option.* ^^ {
      _.toMap
    }

  def option: Parser[(String, Value)] =
    comments.? ~ name ~ value ^^ {
      case comments ~ name ~ value =>
        (name, value.withComments(comments.filter(_.nonEmpty)))
    }

  def stanza: Parser[String] =
    ("global"
     | "defaults"
     | "frontend"
     | "backend"
     | "listen")

  def name: Parser[String] =
    (ws.? ~> not(stanza) ~> id <~ ws.?) ^^ {
      case n => n
    }

  def value: Parser[Value] =
    ws.? ~> ".+".r ^^ {
      case chars => Value.Str(chars.mkString(""))
    }

  def apply(in: String) =
    parseAll(config, in) match {
      case success if (success.successful) => Some(success.get)
      case failure => None
    }
}

object Parse {
  def apply(in: String) = new Parse()(in)
}
