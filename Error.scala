/**
 * records errors.Has only one constructor. In which first argument tells what type of error and second in which position it occured
 */
enum MyError {
  case Error(code : Int, pos : Int)

  /**
   * before toString called prints message in the beginning 
   * @return verbose indication where error occurred
   */
  private def position: String = this match
    case Error(code, pos) => "error at character at position " + pos.toString + ": "

  /**
   * only has 5 types of error. if it encounters more means that error is in a code and should fail program
   * @return error message
   */
  override def toString: String = position + (this match
    case Error(1, _) => "Could not recognize lexeme"
    case Error(2, _) => "Tree expected"
    case Error(3, _) => "Identifier expected"
    case Error(4, _) => "Tree should not start from closing parenthesis"
    case Error(5, _) => "Multiple Trees not allowed"
    case Error(_, _) => "Unknown error")
}

/**
 * Wraps A where in case of error it has recorded
 * @tparam A type to which we apply. In code is just used for non-terminals elements of our language
 */
enum Parsed[A] extends Monad[Parsed, A]{
  case SucceededWith(a : A)
  case HasError(error: MyError)
  override def map[B](f: A => B): Parsed[B] = this match {
    case SucceededWith(c) => SucceededWith(f(c))
    case HasError(err) => HasError(err)
  }
  override def flatMap[B](f: A => Parsed[B]): Parsed[B] = this match
    case SucceededWith(a) => f(a)
    case HasError(err) => HasError(err)

  /**
   * should be used if you are convinced that parsing was successful. Otherwise, program have to fail
   * @return A what was wrapped
   */
  def unsafeGet : A = this match
    case SucceededWith(a) => a
    case _ => ???
}

/**
 * endofunctor
 * needed to proof that this is indeed an endofunctor in category of types
 * @tparam F
 * @tparam A
 */
trait Functor[F[_], A] {
  def map[B](f : A => B) : F[B]
}

/**
 * Monad, but just with flatmap
 * @tparam F endofunctor
 * @tparam A type to which we apply
 */
trait Monad[F[_], A] extends Functor[F, A] {
  def flatMap[B](f : A => F[B]) : F[B]
}
