import Parsed.{HasError, SucceededWith}
import Token.WhiteSpace

import scala.annotation.tailrec
import scala.util.matching.Regex

/**
 * point of this object is to provide this function, which converts sequence of chars to sequence of tokens
 * it is done by first creating regex for each token. Handle functions which lexes each token. Combined lexer which can
 * handle every token. and lexers helping function which tail-recursively computes list of tokens
 */
object Lexer {
  /**
   * point of this object is to provide this function.
   * @param input string to convert to sequence of tokens
   * @return list of consequent tokens with position of its first character, which is needed for more verbose error response
   */
  def lexer(input: String): Parsed[List[(Token, Int)]] = lexerHelper(input, megaLexer, Nil, 0, (s: String) => s == "").
    map((xs: List[(Token, Int)]) => xs.filterNot((y: (Token, Int)) => y._1 == WhiteSpace))

  //regexes for each token
  private val openingParenthesisRegex: Regex = "^\\(".r
  private val closingParenthesisRegex: Regex = "^\\)".r
  private val identifierRegex: Regex = "^[a-zA-Z]+".r
  private val whiteSpaceRegex: Regex = "^\\s+".r
  /**
   * @param input string to lex
   * @return if lexing was successful, it returns token for '(' with it length and unlexed string
   */
  private def handleOpeningParenthesis(input : String) : Option[(Token, Int, String)] =
    openingParenthesisRegex.findFirstIn(input).
      map((s : String) => (Token.OpeningParenthesis, s.length, input.substring(s.length)))

  /**
   * @param input string to lex
   * @return if lexing was successful, it returns token for ')' with it length and unlexed string
   */
  private def handleClosingParenthesis(input: String): Option[(Token, Int, String)] =
    closingParenthesisRegex.findFirstIn(input).
    map((s : String) => (Token.ClosingParenthesis, s.length, input.substring(s.length)))

  /**
   * @param input string to lex
   * @return if lexing was successful, it returns token for whitespace with it length and unlexed string
   */
  private def handleWhiteSpace(input : String) : Option[(Token, Int, String)] =
    whiteSpaceRegex.findFirstIn(input).
    map((s: String) => (Token.WhiteSpace, s.length, input.substring(s.length)))

  /**
   * @param input string to lex
   * @return if lexing was successful, it returns token for identifier consisted with matched string whitespace with it length and unlexed string
   */
  private def handleName(input : String) : Option[(Token, Int, String)] =
    identifierRegex.findFirstIn(input).
    map((s: String) => (Token.ID(s), s.length, input.substring(s.length)))

  /**
   * @param options list of optionals with type A
   * @tparam A generic type
   * @return first none empty optional. If not found, none instead
   */
  private def combine[A](options: List[Option[A]]): Option[A] = options.find((x: Option[A]) => x.isDefined).flatten

  /**
   * it tries successively to lex string until it succeeds. If not returns none
   * @param input string to lex
   * @return if lexing was successful, it returns token which it was able to lex, with it length and unlexed string
   */
  private def megaLexer(input: String) : Option[(Token, Int, String)] = combine(List(handleOpeningParenthesis(input),
    handleClosingParenthesis(input),
    handleWhiteSpace(input),
    handleName(input)))

  /**
   * returns List of lexed tokens from string with position of the first character in it
   * @param input to lex
   * @param lex lexer which lexes one token
   * @param init already existing List of tokens to which we want to append new one
   * @param pos shows which character is now proceeding in lexer
   * @param success to check whether lexing already done
   * @return List of tokens wrapped accordingly to Parsed if succeeded. If not then error with indicating position of start of unknown lexeme
   */
  @tailrec
  private def lexerHelper(input : String, lex : String => Option[(Token, Int, String)], init : List[(Token, Int)], pos : Int, success : String => Boolean) : Parsed[List[(Token, Int)]] =
    if (success(input)) SucceededWith(init)
    else lex(input) match
      case None => HasError(MyError.Error(1, pos))
      case Some(token, i, s) => lexerHelper(s, megaLexer, init.appended(token, pos), pos + i, success)
}