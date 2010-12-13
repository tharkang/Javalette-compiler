import sbt._
import mk262968.JavaCup;

class JavaletteProject(info: ProjectInfo) extends DefaultProject(info) with JavaCup with extract.BasicSelfExtractingProject {
	override def installActions = "update" :: "package" :: Nil
}
