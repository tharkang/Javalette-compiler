package mk262968.javalette;
import scala.annotation.tailrec;

sealed abstract class Type {
  def toString: String;
}
case object UnknownType extends Type {
  override def toString = "* fake type (you should't really see this *"
}
case class SimpleType(name: String) extends Type {
  override def toString = name
}
object VoidType extends SimpleType("void");
object IntType extends SimpleType("int");
object DoubleType extends SimpleType("double");
object StringType extends SimpleType("string");
object BooleanType extends SimpleType("boolean");

class FunctionCallException(val arg: Int, msg: String) extends Exception(msg)

case class FunctionType(val arguments: List[Type], ret: Type) extends Type{
  def call(args: List[Type]): Type = {
    @tailrec
    def f(t: List[Type], a: List[Type], i: Int): Type = t match {
      case Nil => a match {
        case Nil => ret
        case head :: atail => throw new FunctionCallException(0, "Too much arguments");
      }
      case typ :: ttail => a match {
        case Nil => throw new FunctionCallException(0, "Too few arguments");
        case head :: atail =>
          if(head == typ)
            f(ttail, atail, i+1)
          else
            throw new FunctionCallException(i, "Expected argument of type %s not %s".format(head, typ));
      }

    }
    f(arguments, args, 1);
  }
  override def toString = "(%s) -> %s".format(arguments.map(t => t.toString) mkString ", ", ret.toString)
}
