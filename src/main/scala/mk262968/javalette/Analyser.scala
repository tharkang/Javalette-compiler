package mk262968.javalette;
package semantic;

import scala.collection.immutable.HashMap;

class SemanticError(msg: String) extends Error(msg)
class WrongTypeError(msg: String) extends SemanticError("Wrong type: " + msg)
class UndeclaredTypeError(msg: String) extends SemanticError("Undeclared type: " + msg)
class UndeclaredError(msg: String) extends SemanticError("Undeclared variable/function: " + msg)
class RedeclaredError(msg: String) extends SemanticError("Redeclared variable/function: " + msg)
class ReturnError(msg: String) extends SemanticError(msg)

class Environment private (varsStack: List[Map[String, (Type, Int)]], typs:Map[String, Type], toReturn: Type, returned: Boolean, depth: Int) {
	def declare(name: String, typ: Type): Environment = varsStack match {
    case vars :: tail =>
      if(vars.getOrElse(name, (VoidType, -1))._2 == depth)
        throw new RedeclaredError(name)
      else
        new Environment((vars + (name -> (typ, depth))) :: tail, typs, toReturn, returned, depth)
  }
	def lookup(name: String) = 
    try {
      varsStack.head(name)._1
    }
    catch {
      case e: NoSuchElementException => throw new UndeclaredError(name)
    }
	def getType(name: String) = 
    try {
      typs(name)
    }
    catch {
      case e: NoSuchElementException => throw new UndeclaredTypeError(name)
    }
  def inBlock = new Environment(varsStack.head :: varsStack, typs, toReturn, returned, depth+1)
  def outBlock = new Environment(varsStack.tail, typs, toReturn, returned, depth-1)
  def inFunction(rtyp: Type) = new Environment(varsStack, typs, rtyp, false, depth)
  def outFunction: Environment= 
    if(toReturn == null) {
      Main.error("COMPILER ERROR outFunction out of function", (0,0)); this
    }
    else if(!returned)
      throw new ReturnError("Function must return %s, it returns nothing".format(toReturn.toString))
    else
      this
  def ret(typ: Type): Environment =
    if(toReturn == null)
      throw new ReturnError("Return outside of function");
    else if(typ == toReturn)
      new Environment(varsStack, typs, toReturn, true, depth)
    else
      throw new ReturnError("Function must return %s, not %s".format(toReturn.toString, typ.toString));
}

object Environment {
	def builtin = new Environment(
    HashMap(
      "printInt" -> (new FunctionType(List(IntType), VoidType), 0),
      "printDouble" -> (new FunctionType(List(DoubleType), VoidType), 0),
      "printString" -> (new FunctionType(List(StringType), VoidType), 0),
      "readInt" -> (new FunctionType(List.empty[Type], IntType), 0),
      "readDouble" -> (new FunctionType(List.empty[Type], DoubleType), 0)
    ) :: Nil, 
    HashMap(
      "int" -> IntType,
      "double" -> DoubleType,
      "void" -> VoidType,
      "boolean" -> BooleanType
    ), null, false, 0);
}

object Analyser {
	def checkType(found: Type, expected: Type): Unit = {
    if(found != expected && found != UnknownType)
      throw new WrongTypeError("got %s, expected %s".format(found.toString, expected.toString));
	}

	def checkType(node: ast.Ast, expected: Type, env: Environment): Unit = checkType(analyse(node, env)._1, expected)

	def analyseList(nodes: List[ast.Ast], env: Environment): (List[Type], Environment) = {
		val (e, revtypes) = nodes.foldLeft((env, Nil.asInstanceOf[List[Type]])){(el, n) => val (t, ee) = analyse(n, el._1); (ee, t :: el._2)};
		val types = revtypes.reverse;
		(types, e);
	}

  def safelyGetType(name: String, env: Environment, poso: Option[(Int, Int)] = None): Type = 
    try {
      env.getType(name)
    }
    catch {
      case e: UndeclaredTypeError => poso match {
        case Some(pos) => Main.error(e.getMessage, pos);
        case None => ()
      }
      UnknownType
    };

	def extractDeclaration(node: ast.Function, env: Environment): (String, Type, (Int, Int)) = {
    (node.name, new FunctionType(node.arguments.map(arg => safelyGetType(arg.typ.name, env)), safelyGetType(node.typ.name, env)), node.pos)
	}

  def stopErrors(pos: (Int, Int))(f: () => Unit): Unit = 
    try {
      f();
    }
    catch {
      case se: SemanticError =>
        Main.error(se.getMessage, pos);
    }

  def declarePossible(vars: List[(String, Type, (Int, Int))], env: Environment): Environment =
    vars.foldLeft(env) { (ee, vr) =>
      try {
        ee.declare(vr._1, vr._2)
      }
      catch {
        case e: RedeclaredError => Main.error(e.getMessage, vr._3); ee
      }
    }

  def analyse(node: ast.Ast): (Type, Environment) = analyse(node, Environment.builtin)

	def analyse(node: ast.Ast, env: Environment): (Type, Environment) =
		try {
      //println("Started %s anal".format(node.toString));
			val res = analyse_(node, env);
      //println("Ending %s anal".format(node.toString));
      res
		}
		catch {
      case se: SemanticError =>
        Main.error(se.getMessage, node.pos);
        (UnknownType, env)
		}

	def analyse_(node: ast.Ast, env: Environment): (Type, Environment) = node match {
    case ast.EmptyInstruction(pos) =>
      (VoidType, env)
		case ast.Variable(name, pos) =>
      (env.lookup(name), env)

		case ast.StringLiteral(value, pos) =>
      (StringType, env)

		case ast.IntegerLiteral(value, pos) =>
      (IntType, env)

		case ast.DoubleLiteral(value, pos) =>
      (DoubleType, env)

		case ast.BooleanLiteral(value, pos) =>
      (BooleanType, env)

		case ast.LogicalOr(left, right, pos) =>
      stopErrors(left.pos) { () => checkType(left, BooleanType, env) }
      stopErrors(right.pos) {  () => checkType(right, BooleanType, env) }
      (BooleanType, env)

		case ast.LogicalAnd(left, right, pos) =>
      stopErrors(left.pos) { () => checkType(left, BooleanType, env) }
      stopErrors(right.pos) { () => checkType(right, BooleanType, env) }
      (BooleanType, env)

		//case ast.Comparison(left, right, t, pos) =>
     // (BooleanType, env)

		case ast.BinaryOperation(left, right, t, pos) =>
			val tl = analyse(left, env);
			val tr = analyse(right, env);
			checkType(tr._1, tl._1);
      if(t == "==" || t == "!=" || t(0) == '<' || t(0) == '>')
        (BooleanType, env)
      else
        (tl._1, env)

		case ast.UnaryOperation(right, t, pos) =>
			val tr = analyse(right, env);
      if(t == "!") {
        checkType(tr._1, BooleanType)
        (BooleanType, env)
      }
      else {
        if(tr._1 != IntType && tr._1 != DoubleType)
          throw new WrongTypeError("got %s, expected int or double".format(tr._1.toString));
        (tr._1, env)
      }

		case ast.Call(name, arguments, pos) =>
      val ftype = env.lookup(name);
      val atypes = arguments.map(arg => analyse(arg, env)._1);
      ftype match {
        case typ: FunctionType =>
          try {
            (typ.call(atypes), env)
          }
          catch {
            case e: FunctionCallException =>
              if(e.arg == 0)
                Main.error(e.getMessage, pos)
              else
                Main.error(e.getMessage, arguments(e.arg-1).pos);
            (UnknownType, env)
          }
        case typ: Type =>
          Main.error("%s is not callable".format(typ.toString), pos);
          (UnknownType, env)
      }

		case ast.Function(name, arguments, typ, body, pos) =>
      val ftyp = safelyGetType(typ.name, env, Some(pos)) 
      val e1 = env.inFunction(ftyp);
      val args = arguments.map(arg => (arg.name, safelyGetType(arg.typ.name, env, Some(arg.typ.pos)), arg.pos));
      val e2 = declarePossible(args, e1);
      val e3 = analyse(body, e2)._2;
      (VoidType, e3.outFunction)

		case ast.Program(functions, pos) =>
      val funcDefs = functions.map(decl => extractDeclaration(decl, env));
      val e = declarePossible(funcDefs, env);
      functions foreach {f => analyse(f, e)};
      (VoidType, e)

		case ast.Assignement(left, value, pos) =>
      val (typ, e) = analyse(left, env);
      checkType(value, typ, env);
      (typ, e)

		case ast.Increment(left, pos) =>
      stopErrors(left.pos) { () => checkType(left, IntType, env); }
      (IntType, env)

		case ast.Decrement(left, pos) =>
      checkType(left, IntType, env);
      (IntType, env)

		case ast.WhileLoop(condition, body, pos) =>
			 stopErrors(condition.pos) { () => checkType(condition, BooleanType, env); }
       (VoidType, analyse(body, env)._2)

		case ast.ForLoop(init, condition, step, body, pos) =>
			val inEnv = analyse(init, env)._2;
			stopErrors(condition.pos) { () => checkType(condition, BooleanType, inEnv); }
			val e2 = analyse(body, inEnv)._2;
      val e3 = analyse(step, e2)._2;
      (VoidType, e3)

		case ast.IfElse(condition, body, elseBody, pos) =>
			stopErrors(condition.pos) { () => checkType(condition, BooleanType, env); }
			val e1 = analyse(body, env)._2;
      if(elseBody != null)
        (VoidType, analyse(elseBody, e1)._2)
      else
        (VoidType, env)

		case ast.Return(value, pos) =>
      if(value != null)
        (VoidType, env.ret(analyse(value, env)._1))
      else
        (VoidType, env.ret(VoidType))

		case ast.Block(instructions, pos) =>
			(VoidType, analyseList(instructions, env.inBlock)._2.outBlock)

		case ast.Declaration(names, t, pos) =>
      val typ = safelyGetType(t.name, env, Some(t.pos));
      if(typ != UnknownType)
        names foreach {dn => stopErrors(dn.pos) { () => if(dn.value != null) checkType(dn.value, typ, env) }; }
      else
        names foreach {dn => if(dn.value != null) analyse(dn.value, env)};
      (VoidType, declarePossible(names map {dn => (dn.name, typ, dn.pos)}, env))

    case ast: ast.Ast => 
      Main.error("COMPILER ERROR: unexpected node type", ast.pos);
      (VoidType, env)
	}
}
