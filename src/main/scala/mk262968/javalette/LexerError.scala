package mk262968.javalette;

case class LexerError(message: String, line: Int, column: Int) extends Error {
	override def getMessage = message + (if(line != -1) " line %d column %d".format(line, column) else "")
}