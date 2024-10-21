import org.scalatest.funsuite.AnyFunSuite

class LexerTest extends AnyFunSuite{
  private val op = Token.OpeningParenthesis
  private val cp = Token.ClosingParenthesis
  private val ws = Token.WhiteSpace
  private val id0 = Token.ID("a")
  private val id1 = Token.ID("aa")
  private val id2 = Token.ID("b")

  test("op one only test") {
    assertResult(Parsed.SucceededWith(List((op, 0)))) {
      Lexer.lexer("(")
    }
  }
  test("cp one only test") {
    assertResult(Parsed.SucceededWith(List((cp, 0)))) {
      Lexer.lexer(")")
    }
  }
  test("white space one only test") {
    assertResult(Parsed.SucceededWith(Nil)) {
      Lexer.lexer("    ")
    }
  }
  test("id one only test") {
    assertResult(Parsed.SucceededWith(List((id0, 0)))) {
      Lexer.lexer("a")
    }
  }
  test("multiple test") {
    assertResult(Parsed.SucceededWith(List((id1, 0), (op, 2), (cp, 3), (id2, 4)))) {
      Lexer.lexer("aa()b")
    }
  }
  test("deleted white spaces test") {
    assertResult(Parsed.SucceededWith(List((id1, 1), (op, 5), (cp, 12), (id2, 23)))) {
      Lexer.lexer(" aa  (  \n   )          b\n")
    }
  }
  test("first failure test") {
    assertResult(Parsed.HasError(MyError.Error(1, 0))) {
      Lexer.lexer("9aa()b")
    }
  }
  test("middle failure test") {
    assertResult(Parsed.HasError(MyError.Error(1, 6))) {
      Lexer.lexer("aa    90()b")
    }
  }
}
