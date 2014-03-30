package harp

import scala.util.parsing.combinator.RegexParsers

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

case class Config(sections: Seq[Section]) {
  lazy val defaults = sections find(_.name == "defaults")
  lazy val global = sections find(_.name == "global")

  def frontend(name: String) =
    sections find {
      case Section.Frontend(`name`, _, _) => true
      case _ => false
    }

  def backend(name: String) =
    sections find {
      case Section.Backend(`name`, _, _) => true
      case _ => false
    }
}

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

object Main {
  def main(a: Array[String]) {
    val conf = Parse("""
    |# global config goes here
    |global
    |  maxconn 4096
    |  pidfile ~/tmp/haproxy-queue.pid
    |  stats socket /tmp/haproxy.stat mode 600
    |
    |defaults
    | log global
    |  log 127.0.0.1 local0
    |  log 127.0.0.1 local1 notice  
    |  mode http
    |  timeout connect 300000
    |  timeout client 300000
    |  timeout server 300000
    |  option httpchk HEAD / HTTP/1.0
    |
    |frontend http-farm
    |  # binds to port 9000
    |  # then listens of course
    |  bind *:9000
    |  default_backend app1latest
    |  acl url_tag02 path_beg /tag02/
    |  use_backend tagged-02 if url_tag02
    |
    |backend app1latest
    |  balance roundrobin
    |  server localhost_9001 localhost:9001
    |
    |listen haproxyapp_admin:9100 127.0.0.1:9100
    |  mode http
    |  stats uri /""".stripMargin)

    for {
      cfg  <- conf
      fe   <- cfg.frontend("http-farm")
      opt <- fe.get("acl")
    } println(opt)
  }
}
