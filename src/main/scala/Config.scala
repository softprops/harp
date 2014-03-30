package harp

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
