
enum Nodes extends ASTElement[Nodes]{
  case Forest(forest: List[Tree])

  override def toString: String = this match {
    case Forest(Nil) => ""
    case Forest(x :: Nil) => x.toString
    case Forest(x :: xs) => x.toString + " " + Forest(xs).toString
  }

  override def equals(obj: Any): Boolean = (this, obj) match
    case (Forest(Nil), Forest(Nil)) => true
    case (Forest(Nil), _) => false
    case (_, Forest(Nil)) => false
    case (Forest(f1 :: forest1), Forest(f2 :: forest2)) => f1.equals(f2) && Forest(forest1).equals(Forest(forest2))
    case (_, _) => false

  override def replace(treeToReplace : Tree, treeReplaceWith : Tree) : Nodes = this match
    case Forest(xs) => Forest(xs.map((tree : Tree) => tree.replace(treeToReplace, treeReplaceWith)))
}

enum IDs extends ASTElement[IDs] {
  case ID(name : String)

  override def toString: String = this match
    case ID(name) => name

  override def equals(obj: Any): Boolean = (this, obj) match
    case (ID(name1), ID(name2)) => name1.equals(name2)
    case _ => false

  override def replace(treeToReplace : Tree, treeReplaceWith : Tree): IDs = this
}

enum Tree extends ASTElement[Tree] {
  case ID(id: IDs)
  case Node(node: Nodes)

  override def toString: String = this match {
    case ID(name) => name.toString
    case Node(xs) => "(" + xs.toString + ")"
  }

  override def equals(obj: Any): Boolean = (this, obj) match
    case (ID(id1), ID(id2)) => id1.equals(id2)
    case (Node(node1), Node(node2)) => node1.equals(node2)
    case _ => false

  override def replace(treeToReplace: Tree, treeReplaceWith: Tree): Tree = if (this.equals(treeToReplace)) treeReplaceWith else this match
    case ID(x) => ID(x.replace(treeToReplace, treeReplaceWith))
    case Node(x) => Node(x.replace(treeToReplace, treeReplaceWith))
}

trait ASTElement[A] {
  def replace(treeToReplace : Tree, treeReplaceWith : Tree) : A
}
