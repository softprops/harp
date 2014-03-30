package harp

case class Config(sections: Seq[Section]) {
  lazy val defaults = sections find(_.name == Parse.Defaults)

  lazy val global = sections find(_.name == Parse.Global)

  lazy val frontends: Iterable[Section.Frontend] =
    (List.empty[Section.Frontend] /: sections) {
      case (ends, fe: Section.Frontend) => fe :: ends
      case (ends, _) => ends
    }

  lazy val backends: Iterable[Section.Backend] =
    (List.empty[Section.Backend] /: sections) {
      case (ends, be: Section.Backend) => be :: ends
      case (ends, _) => ends
    }

  def frontend(name: String) =
    frontends find {
      case Section.Frontend(fe, _, _) => fe == name
    }

  def backend(name: String) =
    backends find {
      case Section.Backend(be, _, _) => be == name
    }
}
