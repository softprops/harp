package harp

import scala.concurrent.duration.Duration
import scala.util.parsing.combinator.RegexParsers
import java.util.concurrent.TimeUnit

class Parse extends RegexParsers {
  import harp.Parse._

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
    comments.? ~ (Global ~> options) ^^ {
      case comments ~ opts => Section.Global(opts, comments.filter(_.nonEmpty))
    }

  def defaults: Parser[Section.Defaults] =
    comments.? ~ (Defaults ~> options) ^^ {
      case comments ~ opts => Section.Defaults(opts, comments.filter(_.nonEmpty))
    }

  def frontend: Parser[Section.Frontend] =
    comments.? ~ (Frontend ~> name) ~ options ^^ {
      case (comments ~ name ~ opts) => Section.Frontend(name, opts, comments.filter(_.nonEmpty))
    }

  def backend: Parser[Section.Backend] =
    comments.? ~ (Backend ~> name) ~ options ^^ {
      case comments ~ name ~ opts =>
        Section.Backend(name, opts, comments.filter(_.nonEmpty))
    }

  def listener: Parser[Section.Listener] =
    comments.? ~ (Listen ~> name) ~ name.? ~ options ^^ {
      case comments ~ lname ~ addr ~ opts =>
        val hostport = addr.collect {
          case Addr(host, port) =>
            Map("host" -> Value.Str(if (host.isEmpty) "0.0.0.0" else host),
                "port" -> Value.Num(port.toInt))
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
    (Global
     | Defaults
     | Frontend
     | Backend
     | Listen
     | Server)

  def name: Parser[String] =
    (ws.? ~> not(stanza) ~> id <~ ws.?) ^^ {
      case n => n
    }

  def value: Parser[Value] =
    duration | num | str

  def duration: Parser[Value.Duration] =
    ws.? ~> DurationStr ^^ {
      case DurationStr(len, unit) =>
        Value.Duration(Duration(len.toLong, Parse.Durations(unit)))
    }

  def num: Parser[Value.Num] =
    ws.? ~> Digits ^^ {
      case len => Value.Num(len.toInt)
    }

  def str: Parser[Value.Str] =
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
  val Global   = "global"
  val Defaults = "defaults"
  val Frontend = "frontend"
  val Backend  = "backend"
  val Listen   = "listen"
  val Server   = "server"
  val Addr = """(.*):(\d)+""".r
  val Digits = """\d+""".r
  val DurationStr = """(\d+)(us|ms|s|m|h|d)""".r
  val ws = """\s*""".r
  val id = """[0-9A-Za-z-_.:]+""".r
  val comment = """#.+""".r

  val Durations = Map(
    "us" -> TimeUnit.MICROSECONDS,
    "ms" -> TimeUnit.MILLISECONDS,
    "s" -> TimeUnit.SECONDS,
    "m" -> TimeUnit.MINUTES,
    "h" -> TimeUnit.HOURS,
    "d" -> TimeUnit.DAYS)
  def apply(in: String) = new Parse()(in)
}
