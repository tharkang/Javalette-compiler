package mk262968.javalette

import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader

import scala.io.Source

class CompilationError(msg: String) extends Exception(msg)

object Main {
  var current: Option[File] = None;
  var lines: Option[Array[String]] = None;
  var ok = true;
	def compile(inFile: File) = {
    ok = true;
    current = Some(inFile);
		val inStream = new InputStreamReader(new FileInputStream(inFile), "UTF-8");
		val lexer = new ast.JavaletteLexer(inStream);
		val parser = new ast.JavaletteParser(lexer);
		val astRoot = parser.parse().value.asInstanceOf[ast.Ast];
    //println("AST: " + astRoot.toString);
    semantic.Analyser.analyse(astRoot)
    current = None;
    lines = None;
    if(!ok)
      throw new CompilationError("Errors");
	}

	def main(argv: Array[String]): Unit = {
		try {
			compile(new File(argv(0)));
		}
		catch {
			case e:ast.LexerError => error("Lexing error:\t" + e.getMessage, e.pos); System.exit(-1)
			case e:ast.ParserError => error("Parsing error:\t" + e.getMessage, e.pos); System.exit(-1)
		}

	}
  def error(message: String, pos: (Int, Int)) {
    ok = false;
    current match {
      case Some(inFile) =>
        val line = (lines match {
          case Some(la) => la(pos._1-1)
          case None => 
            val la = Source.fromFile(inFile).getLines.toArray;
            lines = Some(la);
            la(pos._1-1)
        }).replaceAll("\t", " ");
        println("Error in %s line %d column %d".format(inFile.getPath.toString, pos._1, pos._2));
        println(line);
        println(" "*(pos._2-1) + "^");
        println(message);
      case None => ()
    }
  }
  def warning(message: String, pos: (Int, Int)) {
    Console.println(message);
  }
}
