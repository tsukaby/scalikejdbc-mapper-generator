package scalikejdbc.mapper

case class GeneratorTemplateType(name: String)

object GeneratorTemplateType {
  val sync = GeneratorTemplateType("sync")
  val async = GeneratorTemplateType("async")
}
