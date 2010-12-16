package mk262968.javalette;
import ast._

import scala.collection.mutable.HashMap
import javax.swing.tree.{TreeModel, TreePath}
import javax.swing.event.TreeModelListener

class AstTreeModel(val root: Ast) extends TreeModel {
	val cache = new HashMap[Ast, List[(Any, String)]]()
	var listeners: List[TreeModelListener] = Nil

	def addTreeModelListener(l: TreeModelListener): Unit =
		listeners = l :: listeners
	
	def first(parent: Any): Any = parent.asInstanceOf[(Any, String)]._1
	def nod(parent: Any): Ast = first(parent).asInstanceOf[Ast]
		

	def getChild(parent: Any, index: Int): AnyRef = 
			packChildren(nod(parent)).drop(index).head	

	def getChildCount(parent: Any): Int = 
			packChildren(nod(parent)).length

	def getIndexOfChild(parent: Any, child: Any): Int = 
		packChildren(nod(parent)).dropWhile(c => c != child).length

	def getRoot(): AnyRef = (root, "root")

	def isLeaf(node: Any): Boolean = first(node).isInstanceOf[Ast]
	
	def removeTreeModelListener(l: TreeModelListener): Unit =
      listeners filterNot (_ == l)

	def valueForPathChanged(path: TreePath, newValue: Any): Unit = {} 

	def packChildren(node: Ast): List[(Any, String)] = cache.get(node) match {
		case Some(x:List[(Any, String)]) =>
			x
		case _ =>
			node.getClass.getDeclaredFields.map(field => (field.get(node), field.getName)).toList;
	}
}
