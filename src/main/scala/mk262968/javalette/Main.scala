package mk262968.javalette

import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader

object Main {
	def compile(path: String) = {
		val inFile = new File(path);
		val inStream = new InputStreamReader(new FileInputStream(inFile), "UTF-8");
		val lexer = new JavaletteLexer(inStream);
		val parser = new JavaletteParser(lexer);
		parser.parse();
	}
	def main(argv: Array[String]): Unit = {
		try {
			compile(argv(0));
		}
		catch {
			case e:LexerError => Console.println("Lexing error:\t" + e.getMessage); System.exit(-1)
			case e:ParserError => Console.println("Parsing error:\t" + e.getMessage); System.exit(-1)
		}

	}
}
