import IDs.ID
import Nodes.Forest
import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite

class TreeTest extends AnyFunSuite {
  private val ida0 : IDs = ID("a")
  private val ida1 : IDs = ID("a")
  private val id1 : IDs = ID("aa")
  private val id2 : IDs = ID("b")
  private val emptyNode : Nodes = Forest(Nil)

  test("simple equal test") {
    assertResult(true) {Tree.ID(ida0).equals(Tree.ID(ida1))}
  }
  test("simple not equal test") {
      assertResult(false) {
        Tree.ID(id1).equals(id2)
      }
  }
  test("ID-NODE equal test") {
      assertResult(false) {
        Tree.ID(id1).equals(Tree.Node(Nodes.Forest(List(Tree.ID(id1)))))
      }
  }
  test("NODE-ID equal test") {
    val tree = Tree.Node(Nodes.Forest(List(Tree.ID(id1))))
      assertResult(false) {
        tree.equals(Tree.ID(id1))
      }
  }
  test("NODE-NODE equal test") {
    val tree0 = Tree.Node(Nodes.Forest(List(Tree.ID(id1))))
    val tree1 = Tree.Node(Nodes.Forest(List(Tree.ID(id1))))
    assertResult(true) {
      tree0.equals(tree1)
    }
  }
  test("NODE-NODE first empty equal test") {
    val tree = Tree.Node(Nodes.Forest(List(Tree.ID(id1))))
    assertResult(false) {
      Tree.Node(emptyNode).equals(tree)
    }
  }
  test("NODE-NODE both empty equal test") {
    assertResult(true) {
      Tree.Node(emptyNode).equals(Tree.Node(emptyNode))
    }
  }
  test("NODE-NODE second empty equal test") {
    val tree = Tree.Node(Nodes.Forest(List(Tree.ID(id1))))
    assertResult(false) {
      tree.equals(Tree.Node(emptyNode))
    }
  }
  test("NODE-NODE different size equal test1") {
    val tree0 = Tree.Node(Nodes.Forest(List(Tree.ID(id1))))
    val tree1 = Tree.Node(Nodes.Forest(List(Tree.ID(id1), Tree.ID(id1))))
    assertResult(false) {
      tree0.equals(tree1)
    }
  }
  test("NODE-NODE different size equal test2") {
    val tree0 = Tree.Node(Nodes.Forest(List(Tree.ID(id1))))
    val tree1 = Tree.Node(Nodes.Forest(List(Tree.ID(id1), Tree.ID(id1))))
    assertResult(false) {
      tree1.equals(tree0)
    }
  }
  test("NODE-NODE first post same equal test") {
    val tree0 = Tree.Node(Nodes.Forest(List(Tree.ID(id1), Tree.ID(id2))))
    val tree1 = Tree.Node(Nodes.Forest(List(Tree.ID(id1), Tree.ID(id1))))
    assertResult(false) {
      tree1.equals(tree0)
    }
  }
  test("NODE-NODE same length equal test") {
    val tree0 = Tree.Node(Nodes.Forest(List(Tree.ID(id1), Tree.ID(id2))))
    val tree1 = Tree.Node(Nodes.Forest(List(Tree.ID(id1), Tree.ID(id2))))
    assertResult(true) {
      tree1.equals(tree0)
    }
  }
  test("NODE-NODE shuffled equal test") {
    val tree0 = Tree.Node(Nodes.Forest(List(Tree.ID(id2), Tree.ID(id1))))
    val tree1 = Tree.Node(Nodes.Forest(List(Tree.ID(id1), Tree.ID(id2))))
    assertResult(false) {
      tree1.equals(tree0)
    }
  }
  test("nested node equals") {
    val tree0 = Tree.Node(Nodes.Forest(List(Tree.Node(emptyNode), Tree.ID(ida1))))
    val tree1 = Tree.Node(Nodes.Forest(List(Tree.Node(emptyNode), Tree.ID(ida1))))
    assertResult(true) {
      tree0.equals(tree1)
    }
  }

  test("TREE-ID equal test") {
    val tree0 = Tree.Node(Nodes.Forest(List(Tree.ID(id1))))
    val tree1 = Tree.Node(Nodes.Forest(List(Tree.ID(id1), Tree.ID(id2))))
    assertResult(false) {
      tree0.equals(id1)
    }
  }
  test("TREE-NODE equal test") {
    val tree0 = Tree.Node(emptyNode)
    assertResult(false) {
      tree0.equals(emptyNode)
    }
  }
  test("NODEs equal test") {
    val node0 = Nodes.Forest(List(Tree.ID(id1)))
    val node1 = Nodes.Forest(List(Tree.ID(id1)))
    assertResult(true) {
      node0.equals(node1)
    }
  }
  test("NODEs first empty equal test") {
    val node = Nodes.Forest(List(Tree.ID(id1)))
    assertResult(false) {
      emptyNode.equals(node)
    }
  }
  test("NODEs both empty equal test") {
    assertResult(true) {
      emptyNode.equals(emptyNode)
    }
  }
  test("NODEs second empty equal test") {
    val node = Nodes.Forest(List(Tree.ID(id1)))
    assertResult(false) {
      node.equals(emptyNode)
    }
  }
  test("NODEs different size equal test1") {
    val node0 = Nodes.Forest(List(Tree.ID(id1)))
    val node1 = Nodes.Forest(List(Tree.ID(id1), Tree.ID(id1)))
    assertResult(false) {
      node0.equals(node1)
    }
  }
  test("NODEs different size equal test2") {
    val node0 = Nodes.Forest(List(Tree.ID(id1)))
    val node1 = Nodes.Forest(List(Tree.ID(id1), Tree.ID(id1)))
    assertResult(false) {
      node1.equals(node0)
    }
  }
  test("NODEs first post same equal test") {
    val node0 = Nodes.Forest(List(Tree.ID(id1), Tree.ID(id2)))
    val node1 = Nodes.Forest(List(Tree.ID(id1), Tree.ID(id1)))
    assertResult(false) {
      node1.equals(node0)
    }
  }
  test("NODEs same length equal test") {
    val node0 = Nodes.Forest(List(Tree.ID(id1), Tree.ID(id2)))
    val node1 = Nodes.Forest(List(Tree.ID(id1), Tree.ID(id2)))
    assertResult(true) {
      node1.equals(node0)
    }
  }
  test("NODEs shuffled equal test") {
    val node0 = Nodes.Forest(List(Tree.ID(id2), Tree.ID(id1)))
    val node1 = Nodes.Forest(List(Tree.ID(id1), Tree.ID(id2)))
    assertResult(false) {
      node1.equals(node0)
    }
  }
  test("simple toString test") {
    val tree = Tree.ID(ida1)
    assertResult("a") {
      tree.toString
    }
  }
  test("empty toString test") {
    val tree = Tree.Node(emptyNode)
    assertResult("()") {
      tree.toString
    }
  }
  test("single toString test") {
    val tree = Tree.Node(Nodes.Forest(List(Tree.ID(ida1))))
    assertResult("(a)") {
      tree.toString
    }
  }
  test("multiple toString test") {
    val tree = Tree.Node(Nodes.Forest(List(Tree.ID(ida1), Tree.ID(ida0),Tree.ID(id1))))
    assertResult("(a a aa)") {
      tree.toString
    }
  }
  test("nested toString test") {
    val tree = Tree.Node(Nodes.Forest(List(Tree.Node(emptyNode), Tree.ID(ida1))))
    assertResult("(() a)") {
      tree.toString
    }
  }
  test("nested toString from example test") {
    val tree = Tree.Node(Nodes.Forest(List(Tree.Node(Nodes.Forest(List(Tree.ID(ida0), Tree.ID(id2)))), Tree.ID(ida1), Tree.ID(id1))))
    assertResult("((a b) a aa)") {
      tree.toString
    }
  }
  test("simple replace test") {
    val tree0 = Tree.ID(ida0)
    val tree1 = Tree.ID(ida1)
    val tree2 = Tree.ID(id1)
    val tree3 = Tree.ID(id1)
    assertResult(tree3) {tree0.replace(tree1, tree2)}
  }
  test("not found replace test") {
    val tree0 = Tree.ID(ida0)
    val tree1 = Tree.ID(id2)
    val tree2 = Tree.ID(id1)
    val tree3 = Tree.ID(id1)
    assertResult(tree0) {
      tree0.replace(tree1, tree2)
    }
  }
  test("empty replace test") {
    val tree0 = Tree.Node(emptyNode)
    val tree1 = Tree.Node(emptyNode)
    val tree2 = Tree.ID(id1)
    val tree3 = Tree.ID(id1)
    assertResult(tree3) {
      tree0.replace(tree1, tree2)
    }
  }
  test("multiple tree in node replace test") {
    val tree0 = Tree.Node(Nodes.Forest(List(Tree.ID(ida0), Tree.ID(id1))))
    val tree1 = Tree.Node(Nodes.Forest(List(Tree.ID(ida0), Tree.ID(id1))))
    val tree2 = Tree.ID(id1)
    val tree3 = Tree.ID(id1)
    assertResult(tree3) {
      tree0.replace(tree1, tree2)
    }
  }
  test("partially replace test") {
    val tree0 = Tree.Node(Nodes.Forest(List(Tree.ID(ida0), Tree.ID(id1))))
    val tree1 = Tree.ID(ida0)
    val tree2 = Tree.ID(id1)
    val tree3 = Tree.Node(Nodes.Forest(List(Tree.ID(id1), Tree.ID(id1))))
    assertResult(tree3) {
      tree0.replace(tree1, tree2)
    }
  }
  test("deep replace test") {
    val tree0 = Tree.Node(Nodes.Forest(List(Tree.Node(Nodes.Forest(List(Tree.ID(ida0), Tree.ID(id2)))), Tree.ID(id1))))
    val tree1 = Tree.ID(ida0)
    val tree2 = Tree.ID(id1)
    val tree3 =  Tree.Node(Nodes.Forest(List(Tree.Node(Nodes.Forest(List(Tree.ID(id1), Tree.ID(id2)))), Tree.ID(id1))))
    assertResult(tree3) {
      tree0.replace(tree1, tree2)
    }
  }
  test("multiple replace test") {
    val tree0 = Tree.Node(Nodes.Forest(List(Tree.Node(Nodes.Forest(List(Tree.ID(ida0), Tree.ID(id2)))), Tree.ID(ida0))))
    val tree1 = Tree.ID(ida0)
    val tree2 = Tree.ID(id1)
    val tree3 = Tree.Node(Nodes.Forest(List(Tree.Node(Nodes.Forest(List(Tree.ID(id1), Tree.ID(id2)))), Tree.ID(id1))))
    assertResult(tree3) {
      tree0.replace(tree1, tree2)
    }
  }
  test("replace with horrible test") {
    val tree0 = Tree.Node(Nodes.Forest(List(Tree.Node(Nodes.Forest(List(Tree.ID(ida0), Tree.ID(id2)))), Tree.ID(ida0))))
    val tree1 = Tree.ID(ida0)
    val tree2 = Tree.Node(Nodes.Forest(List(Tree.Node(Nodes.Forest(List(Tree.ID(ida0), Tree.ID(id2)))), Tree.ID(ida1), Tree.ID(id1))))
    val tree3 = Tree.Node(Nodes.Forest(List(Tree.Node(Nodes.Forest(List(tree2, Tree.ID(id2)))), tree2)))
    assertResult(tree3) {
      tree0.replace(tree1, tree2)
    }
  }
}
