import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
	val mk262968Repo = "mk262968 repo" at "http://students.mimuw.edu.pl/~mk262968/maven"
  val javacupPlugin = "mk262968" % "java_cup" % "1.0"
	val extract = "org.scala-tools.sbt" % "installer-plugin" % "0.3.0"
}
