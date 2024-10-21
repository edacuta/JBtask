import IDs.ID
import Nodes.Forest
import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite{
  private val ida0 : IDs = ID("a")
  private val ida1 : IDs = ID("a")
  private val id1 : IDs = ID("aa")
  private val id2 : IDs = ID("b")
  private val emptyNode : Nodes = Forest(Nil)

  test("simple parser test") {
    assertResult(Parsed.SucceededWith(Tree.ID(ida0))) {
      Parser.predictiveParser("a")
    }
  }
  test("empty parser test") {
    assertResult(Parsed.SucceededWith(Tree.Node(emptyNode))) {
      Parser.predictiveParser("()")
    }
  }
  test("single parser test") {
    assertResult(Parsed.SucceededWith(Tree.Node(Nodes.Forest(List(Tree.ID(ida0)))))) {
      Parser.predictiveParser("(a)")
    }
  }
  test("nested parser test") {
    val tree = Tree.Node(Nodes.Forest(List(Tree.Node(Nodes.Forest(List(Tree.ID(ida0), Tree.ID(id2)))), Tree.ID(ida1), Tree.ID(id1))))
    assertResult(Parsed.SucceededWith(tree)) {
      Parser.predictiveParser("((a b) a aa)")
    }
  }
  test("multiple trees not allowed parser test") {
    assertResult(Parsed.HasError(MyError.Error(5, 6))) {
      Parser.predictiveParser("(a b) a aa")
    }
  }
  test("empty string parser test") {
    assertResult(Parsed.HasError(MyError.Error(2, 0))) {
      Parser.predictiveParser("")
    }
  }
  test("tree expected test parser test") {
    assertResult(Parsed.HasError(MyError.Error(2, 0))) {
      Parser.predictiveParser(")")
    }
  }
}
