package harp

import scala.util.parsing.combinator.RegexParsers

sealed trait Value
object Value {
  case class Str(value: String) extends Value
}

case class Config(sections: Seq[Section]) {
  lazy val defaults = sections find(_.name == "defaults")
  lazy val global = sections find(_.name == "global")

  def frontend(name: String) =
    sections find {
      case Section.Frontend(`name`, _) => true
      case _ => false
    }

  def backend(name: String) =
    sections find {
      case Section.Backend(`name`, _) => true
      case _ => false
    }
}

sealed trait Section {
  def name: String
  def options: Map[String, Value]
  def get = options.get(_)
}

object Section {
  abstract class Named(val name: String) extends Section
  case class Defaults(val options: Map[String, Value]) extends Named("defaults") {
    def set(value: (String, Value)) = copy(options = options + value)
  }
  case class Global(val options: Map[String, Value]) extends Named("global") {
    def set(value: (String, Value)) = copy(options = options + value)
  }
  case class Frontend(name: String, options: Map[String, Value]) extends Section {
    def set(value: (String, Value)) = copy(options = options + value)
  }
  case class Backend(name: String, options: Map[String, Value]) extends Section {
    def set(value: (String, Value)) = copy(options = options + value)
  }
  case class Server(name: String, options: Map[String, Value]) extends Section {
    def set(value: (String, Value)) = copy(options = options + value)
  }
  case class Listener(name: String, options: Map[String, Value]) extends Section {
    def set(value: (String, Value)) = copy(options = options + value)
  }
}

class Parse extends RegexParsers {

  def ws: Parser[String] = """\s*""".r

  def id: Parser[String] = """[0-9A-Za-z-_.:]+""".r

  def config: Parser[Config] = section.* ^^ {
    case sections => Config(sections)
  }

  def section: Parser[Section] =
    global | defaults | frontend | backend | listener

  def global: Parser[Section.Global] =
    ("global" ~> options) ^^ {
      case opts => Section.Global(opts)
    }

  def defaults: Parser[Section.Defaults] =
    ("defaults" ~> options) ^^ {
      case opts => Section.Defaults(opts)
    }

  def frontend: Parser[Section.Frontend] =
    ("frontend" ~> name) ~ options ^^ {
      case (name ~ opts) => Section.Frontend(name, opts)
    }

  def backend: Parser[Section.Backend] =
    ("backend" ~> name) ~ options ^^ {
      case (name ~ opts) => Section.Backend(name, opts)
    }

  def listener: Parser[Section.Listener] =
    ("listen" ~> name) ~ name.? ~ options ^^ {
      case (lname ~ addr ~ opts) =>        
        val hostport = addr.flatMap {
          _.split(":", 2) match {
            case Array(host, port) => Some(Map(
              "host" -> Value.Str(host),
              "port" -> Value.Str(port)
            ))
            case _ => None
          }
        }
        Section.Listener(lname, opts ++ hostport.getOrElse(Map.empty[String, Value]))
    }

  //def server: Parser[Server] = */

  def options: Parser[Map[String, Value]] =
    option.* ^^ {
      _.toMap
    }

  def option: Parser[(String, Value)] =
    name ~ value ^^ {
      case (name ~ value) => (name, value)
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
      bind <- fe.get("bind")
    } println(bind)
  }
}
