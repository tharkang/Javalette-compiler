package mk262968.javalette

import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader



object Main {
	def compile(path: String) = {
		val inFile = new File(path);
		val inStream = new InputStreamReader(new FileInputStream(inFile), "UTF-8");
		val lexer = new ast.JavaletteLexer(inStream);
		val parser = new ast.JavaletteParser(lexer);
		val astRoot = parser.parse().value.asInstanceOf[ast.Ast];
    println("AST: " + astRoot.toString);
    semantic.Analyser.analyse(astRoot)
	}
	def main(argv: Array[String]): Unit = {
		try {
			compile(argv(0));
		}
		catch {
			case e:ast.LexerError => Console.println("Lexing error:\t" + e.getMessage); System.exit(-1)
			case e:ast.ParserError => Console.println("Parsing error:\t" + e.getMessage); System.exit(-1)
		}

	}
  def error(message: String, pos: (Int, Int)) {
    Console.println(message);
  }
  def warning(message: String, pos: (Int, Int)) {
    Console.println(message);
  }
}
