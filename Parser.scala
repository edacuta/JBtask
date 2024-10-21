import Parsed.{HasError, SucceededWith}

import scala.annotation.tailrec

/**
 * aim of this object is to create parser which translates string of tokens to suitable data type for our grammar.
 * With implemented lexer by its composition and flattening if there are some errors. We can implement function which
 * takes string and transforms it to Tree. By overriding previous one
 */
object Parser {
  /**
   * parses string by calling lexer and composing it with function for parsing tokens
   * @param input string to parse
   * @return Parsed Tree
   */
  def predictiveParser(input : String) : Parsed[Tree] = Lexer.lexer(input).flatMap((tokens: List[(Token, Int)]) => predictiveParser(tokens))

  /**
   * Parses Tree by calling handle of starting symbol, and composes with checking function
   * FOr this grammar we can use predictive parsing technique, since the only alternative in our is TREE -> ID|NODE
   * both following statements are false ID=>empty NODE =>empty(we cannot derive empty string from them), and that
   * First(ID) /\ First(NODE) = 0(intersection is empty)
   * @param tokens List of tokens which had been return by lexer
   * @return Parsed Tree
   */
  def predictiveParser(tokens: List[(Token, Int)]) : Parsed[Tree] = handleTree(tokens).flatMap(checkForSuccess)

  /**
   * after calling handle of our starting symbol. We expect to parse whole List of tokens. We need function to return just Tree
   * or error if it did not
   * @param t resulting Tree
   * @param xs List of tokens, which should be Nul
   * @return in case of parsing whole List we declare success. If not, Error which says that we do not accept more than
   *         one Tree
   */
  private def checkForSuccess(t : Tree, xs : List[(Token, Int)]) : Parsed[Tree] = xs match
    case Nil => SucceededWith(t)
    case (x, pos) :: xs => HasError(MyError.Error(5, pos))

  /**
   * if argument was Nil, that means that string was empty, but we expect tree instead, as well as if it is something else
   * but an identifier, because First(Tree) = {ID, OpeningParenthesis}
   * @param tokens List of pairs (Token, Int) second member needed to follow where was error
   * @return resulting Tree and remaining list of tokens
   */
  private def handleTree(tokens: List[(Token, Int)]) : Parsed[(Tree, List[(Token, Int)])] = tokens match
    case Nil => HasError(MyError.Error(2, 0))
    case (Token.ID(name), pos) :: xs => handleID(tokens).map((id, xs) => (Tree.ID(id),xs))
    case (Token.OpeningParenthesis, _) :: xs => handleNodes(xs, Nil).map((n, xs) => (Tree.Node(n), xs))
    case (_, pos) :: _ => HasError(MyError.Error(2, pos))


  /**
   * handles ID. if first token is not in {ID} then the error must return since we expect Identifier
   * @param tokens List of pairs (Token, Int) second member needed to follow where was error
   * @return  resulting IDs and remaining list of tokens
   */
  private def handleID(tokens:List[(Token, Int)]) : Parsed[(IDs, List[(Token, Int)])] = tokens match
    case Nil => HasError(MyError.Error(3, 0))
    case (Token.ID(name), pos) :: xs => SucceededWith(IDs.ID(name), xs)
    case (_, pos) :: xs => HasError(MyError.Error(3, pos))

  /**
   * handles Nodes. if first token is not in {Opening Parenthesis} then the error must return since we expect Node
   * @param tokens List of pairs (Token, Int) second member needed to follow where was error
   * @return resulting Nodes and remaining list of tokens
   */
  @tailrec
  private def handleNodes(tokens: List[(Token, Int)], init : List[Tree]) : Parsed[(Nodes, List[(Token, Int)])] = tokens match
    case Nil => HasError(MyError.Error(2, 0))
    case (Token.ClosingParenthesis, _) :: xs => SucceededWith(Nodes.Forest(init), xs)
    case xs => handleTree(xs) match
      case SucceededWith(t, xs) => handleNodes(xs, init.appended(t))
      case HasError(error) =>  HasError(error)
}
