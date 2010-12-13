package mk262968.javalette;
import java.lang.{Integer, String, Double, Boolean};

sealed abstract class Ast()
class Instruction() extends Ast
class Expression() extends Instruction
class LValue() extends Expression
case class Variable(name: String) extends LValue
case class StringLiteral(value: String) extends Expression
case class IntegerLiteral(value: Integer) extends Expression
case class DoubleLiteral(value: Double) extends Expression
case class BooleanLiteral(value: Boolean) extends Expression
case class LogicalOr(left: Expression, right: Expression) extends Expression
case class LogicalAnd(left: Expression, right: Expression) extends Expression
case class Comparison(left: Expression, right: Expression, t: String) extends Expression
case class BinaryOperation(left: Expression, right: Expression, t: String) extends Expression
case class UnaryOperation(right: Expression, t: String) extends Expression
case class Call(name: String, arguments: List[Expression]) extends Expression;

case class Type(name: String) extends Ast
case class Argument(name: String, t: Type) extends Ast
case class Function(name: String, arguments: List[Argument], t: Type, body: Block) extends Ast
case class Program(functions: List[Function]) extends Ast

case class Assignement(left: LValue, value: Expression) extends LValue
case class Increment(left: LValue) extends LValue
case class Decrement(left: LValue) extends LValue
case class WhileLoop(condition: Expression, body: Instruction) extends Instruction
case class ForLoop(init: Assignement, condition:Expression, step:Assignement, body: Instruction) extends Instruction
case class IfElse(condition:Expression, body: Instruction, elseBody: Instruction) extends Instruction
case class Return(value: Expression) extends Instruction
case class Block(instructions: List[Instruction]) extends Instruction;
case class Declaration(names: List[DeclarationName], t: Type) extends Instruction
case class DeclarationName(name: String, value: Expression) extends Instruction


object ListHelper {
	def nil: List[Any] = Nil
	def add(value: Any, l: List[Any]) = value::l
	def one(value: Any) = value::Nil
};
