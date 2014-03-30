package harp

import scala.util.parsing.combinator.RegexParsers

sealed trait Value
object Value {
  case class Str(value: String) extends Value
}

sealed trait Config {
  def name: String
  def options: Map[String, Value]
}

object Config {
  abstract class Named(val name: String) extends Config
  case class Defaults(val options: Map[String, Value]) extends Named("defaults")
  case class Global(val options: Map[String, Value]) extends Named("global")
  case class Frontend(name: String, options: Map[String, Value]) extends Config
  case class Backend(name: String, options: Map[String, Value]) extends Config
  case class Server(name: String, options: Map[String, Value]) extends Config
  case class Listener(name: String, options: Map[String, Value]) extends Config
}

class Parse extends RegexParsers {

  def ws: Parser[String] = """\s*""".r

  def any: Parser[String] = """.|(\r?\n)+""".r

  def id: Parser[String] = """[0-9A-Za-z-_.:]+""".r

  def configs: Parser[Seq[Config]] = config.*

  def config: Parser[Config] =
    global | defaults | frontend | backend | listener

  def global: Parser[Config.Global] =
    ("global" ~> options) ^^ {
      case opts => Config.Global(opts)
    }

  def defaults: Parser[Config.Defaults] =
    ("defaults" ~> options) ^^ {
      case opts => Config.Defaults(opts)
    }

  def frontend: Parser[Config.Frontend] =
    ("frontend" ~> name) ~ options ^^ {
      case (name ~ opts) => Config.Frontend(name, opts)
    }

  def backend: Parser[Config.Backend] =
    ("backend" ~> name) ~ options ^^ {
      case (name ~ opts) => Config.Backend(name, opts)
    }

  def listener: Parser[Config.Listener] =
    ("listen" ~> name) ~ name.? ~ options ^^ {
      case (lname ~ hostport ~ opts) =>
        val hostportOpts = hostport.map(_.split(":", 2) match {
          case Array(host, port) =>
            Map("host" -> Value.Str(host),
                "port" -> Value.Str(port))
          case _ => Map.empty[String, Value]
        }).getOrElse(Map.empty[String, Value])
        Config.Listener(lname, opts ++ hostportOpts)
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
    parseAll(configs, in) match {
      case success if (success.successful) => Some(success.get)
      case failure => None
    }
}

object Parse {
  def apply(in: String) = new Parse()(in)
}

object Main {
  def main(a: Array[String]) {
    println(Parse("""
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
    |listen haproxyapp_admin:9100 127.0.0.1:9100
    |  mode http
    |  stats uri /""".stripMargin))
  }
}
