package mk262968.javalette.ast;
import java.lang.{Integer, String, Double, Boolean};

sealed abstract class Ast()
class Instruction() extends Ast
class Expression() extends Instruction
class LValue() extends Expression
case class Variable(val name: String, val pos: (Int, Int)) extends LValue
case class StringLiteral(val value: String, val pos: (Int, Int)) extends Expression
case class IntegerLiteral(val value: Integer, val pos: (Int, Int)) extends Expression
case class DoubleLiteral(val value: Double, val pos: (Int, Int)) extends Expression
case class BooleanLiteral(val value: Boolean, val pos: (Int, Int)) extends Expression
case class LogicalOr(val left: Expression, val right: Expression, val pos: (Int, Int)) extends Expression
case class LogicalAnd(val left: Expression, val right: Expression, val pos: (Int, Int)) extends Expression
case class Comparison(val left: Expression, val right: Expression, val t: String, val pos: (Int, Int)) extends Expression
case class BinaryOperation(val left: Expression, val right: Expression, val t: String, val pos: (Int, Int)) extends Expression
case class UnaryOperation(val right: Expression, val t: String, val pos: (Int, Int)) extends Expression
case class Call(val name: String, val arguments: List[Expression], val pos: (Int, Int)) extends Expression;

case class Type(val name: String, val pos: (Int, Int)) extends Ast
case class Argument(val name: String, val t: Type, val pos: (Int, Int)) extends Ast
case class Function(val name: String, val arguments: List[Argument], val t: Type, val body: Block, val pos: (Int, Int)) extends Ast
case class Program(val functions: List[Function], val pos: (Int, Int)) extends Ast

case class Assignement(val left: LValue, val value: Expression, val pos: (Int, Int)) extends LValue
case class Increment(val left: LValue, val pos: (Int, Int)) extends LValue
case class Decrement(val left: LValue, val pos: (Int, Int)) extends LValue
case class WhileLoop(val condition: Expression, val body: Instruction, val pos: (Int, Int)) extends Instruction
case class ForLoop(val init: Assignement, val condition:Expression, val step:Assignement, val body: Instruction, val pos: (Int, Int)) extends Instruction
case class IfElse(val condition:Expression, val body: Instruction, val elseBody: Instruction, val pos: (Int, Int)) extends Instruction
case class Return(val value: Expression, val pos: (Int, Int)) extends Instruction
case class Block(val instructions: List[Instruction], val pos: (Int, Int)) extends Instruction;
case class Declaration(val names: List[DeclarationName], val t: Type, val pos: (Int, Int)) extends Instruction
case class DeclarationName(val name: String, val value: Expression, val pos: (Int, Int)) extends Instruction


object ListHelper {
	def nil: List[Any] = Nil
	def add(value: Any, l: List[Any]) = value::l
	def one(value: Any) = value::Nil
};

object PosHelper {
  def pos(line: Int, column: Int) = (line, column)
}
