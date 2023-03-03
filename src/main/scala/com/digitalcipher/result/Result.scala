package com.digitalcipher.result

import scala.annotation.tailrec
import scala.util.Try

/**
 * Functional approach to managing operations that could fail. The result
 * of such an operation is modelled as an immutable [[Result]]. The [[Result]] is
 * parameterized with a success type [[S]], and a failure type [[F]].
 *
 * The [[Result]] class serves as the base class, with sub-types
 * [[Success]] and [[Failure]] that model a successful and failed
 * operation, respectively. For example, to represent a successful
 * operation,
 * {{{
 *   Success(10)
 * }}}
 * To represent a failed operation
 * {{{
 *   Failure("oops")
 * }}}
 *
 * The [[Result]] class implements a number of operations useful for
 * managing the results of an operation. For example,
 * {{{
 *   Success(10).map(_ * 10).getOrElse(5) == 100    // is true
 * }}}
 * multiplies the value, 10, of the success by 10 and if the result is a
 * success, returns 100, otherwise returns 5. In this case, becuase we
 * start with a success, it will return 100.
 *
 * In the above example, the "map" function returns a new [[Result]] object.
 * It does **not** mutate the original [[Result]].
 *
 * The [[Result]] class is success-biased, meaning that most the methods
 * of the [[Result]] class apply to [[Success]] and does nothing to a [[Failure]].
 * For example,
 * {{{
 *   Failure[Int, String]("oops").map(_ * 10).getOrElse("bummer") == "bummer"     // is true
 * }}}
 * because the "map" function returns the failure, and the "getOrElse" returns
 * the specified default value when the result is a [[Failure]].
 *
 * This class is based on [[scala.util.Either]].
 *
 * @tparam S The type of the success value
 * @tparam F The type of the failure value
 *
 * @see [[Success]]
 * @see [[Failure]]
 */
sealed abstract class Result[+S, +F] extends Serializable:

  /**
   * The [[Result]] class is success-biased (aren't we all). This means that most
   * of the functions operate on the values of a [[Success]], merely passing through
   * the values of a [[Failure]]. What this means is that you can't map the failure
   * value, or filter on the failure value, etc.
   *
   * The failure projection returns a failure-biased result. This means that
   * on a projection, you can apply maps, filters, etc on the [[Failure]], while
   * leaving the values of a [[Success]] unchanged. For example,
   * {{{
   *   Failure(10).projection.map(_ => 11) == Failure(11)    // is true
   * }}}
   * while
   * {{{
   *   Success(10).projection.map(_ => 11) == Success(10)   // is also true
   * }}}
   * @return A [[Result.FailureProjection]]
   */
  def projection = Result.FailureProjection(this)

  /**
   * Applies the function [[successFn]] to the value when this is a
   * [[Success]]. Otherwise, applies the [[failureFn]] to the value.
   * Both the [[successFn]] and the [[failureFn]] must have the same
   * return type [[C]].
   *
   * This function is useful when you want to convert the value of the
   * [[Result]] to some type regardless of whether the [[Result]] is
   * a [[Success]] or [[Failure]]. For example, suppose that a function
   * returns a [[Result]][Int, String] and we want to return "yay" when
   * the function succeeds and "boo" when it doesn't:
   * {{{
   *   someFunctionCall().fold(_ => "yay", _ => "boo")
   * }}}
   *
   * @param successFn The function to call when this is a success
   * @param failureFn The function to call when this is failure
   * @tparam C The type of the combined result
   * @return A combined value
   */
  def fold[C](successFn: S => C, failureFn: F => C): C = this match
    case Success(success) => successFn(success)
    case Failure(failure) => failureFn(failure)

  /**
   * Swaps the a [[Success]] to a [[Failure]] with the same value as
   * the [[Success]], or a [[Failure]] to a [[Success]] with the same
   * value as the [[Failure]]. This function is useful when you expect
   * something to fail and would like to continue only when it does
   * fail.
   * @return A [[Result]] whose [[Success]]/[[Failure]] is swapped for
   *         a [[Failure]]/[[Success]]
   */
  def swap: Result[F, S] = this match
    case Success(success) => Failure(success)
    case Failure(failure) => Success(failure)

  /**
   * Function that allows for side effects. For example you can use this
   * method to write to standard-out, or to a file.
   * @param effectFn The function that performs the side effect, and returns n Unit
   * @tparam U The return type of the function
   */
  def foreach[U](effectFn: S => U): Unit = this match
    case Success(success) =>
      try
        effectFn(success)
      catch
        case _: Exception =>
    case _ =>

  /**
   * For a [[Success]] returns the success value. For a [[Failure]] returns the value
   * supplied by the specified [[or]] function.
   * @param or A supplier that returns the value when the [[Result]] is a failure
   * @tparam S1 The return type
   * @return The success value or the specified value on failure
   */
  def getOrElse[S1 >: S](or: => S1): S1 = this match
    case Success(success) => success
    case _ => or

  /**
   * When the [[Result]] is a [[Success]] then returns this [[Result]]. On [[Failure]]
   * returns the [[Result]] from the specified supplier function.
   * @param or The supplier function that returns a [[Result]] when this [[Result]] is a [[Failure]]
   * @tparam F1 The type of the failure value when this is a [[Failure]]
   * @tparam S1 The type of the success value when this is a [[Success]]
   * @return This [[Result]] when it is a [[Success]]; otherwise the [[Result]] returned
   *         from the specified supplier function
   */
  def orElse[F1 >: F, S1 >: S](or: => Result[S1, F1]): Result[S1, F1] = this match
    case Success(_) => this
    case _ => or

  /**
   *
   * @param elem
   * @tparam S1
   * @return
   */
  def contains[S1 >: S](elem: S1): Boolean = this match
    case Success(success) => success == elem
    case _ => false

  def forall(predicate: S => Boolean): Boolean = this match
    case Success(success) => predicate(success)
    case _ => true

  def exists(predicate: S => Boolean): Boolean = this match
    case Success(success) => predicate(success)
    case _ => false

  def flatMap[S1 >: S, F1 >: F](f: S => Result[S1, F1]): Result[S1, F1] = this match
    case Success(success) => f(success)
    case _ => this.asInstanceOf[Result[S1, F1]]

  def flatten[S1 >: S, F1 >: F](implicit ev: S <:< Result[S1, F1]): Result[S1, F1] = flatMap(ev)

  def map[S1](f: S => S1): Result[S1, F] = this match
    case Success(success) => Success(f(success))
    case _ => this.asInstanceOf[Result[S1, F]]

  def filterOrElse[F1 >: F](predicate: S => Boolean, defaultValue: => F1): Result[S, F1] = this match
    case Success(success) if !predicate(success) => Failure(defaultValue)
    case _ => this

  def toOption: Option[S] = this match
    case Success(success) => Some[S](success)
    case _ => None

  /**
   * Converts the [[Result]] to a [[Try]]. For this to work, the failure
   * type [[F]] must be a subtype of [[Throwable]].
   * @param exception The implicit exception value
   * @return A [[Try]]
   */
  def toTry(implicit exception: F <:< Throwable): Try[S] = this match
    case Success(success) => scala.util.Success(success)
    case Failure(failure) => scala.util.Failure(failure)

  def toEither: Either[F, S] = this match
    case Success(success) => Right(success)
    case Failure(failure) => Left(failure)

  def isSuccess: Boolean
  def isFailure: Boolean
end Result

final case class Success[+S, +F](value: S) extends Result[S, F]:
  override def isSuccess: Boolean = true

  override def isFailure: Boolean = false

  /**
   * Up-casts the [[Success]] to a [[Result]]
   * {{{
   *   Success("x")                  // Result[String, Nothing]
   *   Success("x").withFailure[Int] // Result[String, Int]
   * }}}
   * This function is equivalent to
   * {{{
   *   Success[String, Int]("x")    // Result[String, Int]
   * }}}
   *
   * @tparam F1
   * @return
   */
  def withFailure[F1 >: F]: Result[S, F1] = this

end Success

sealed abstract class BaseFailure[+S, +F] extends Result[S, F]:
  override def isSuccess: Boolean = false

  override def isFailure: Boolean = true

  /**
   * Up-casts the `Failure[S, F]` to `Result[S1, F]`.
   * {{{
   *   Failure("x")                  // Result[Nothing, String]
   *   Failure("x").withSuccess[Int] // Result[Int, String]
   * }}}
   *
   * @tparam S1
   * @return
   */
  def withSuccess[S1 >: S]: Result[S1, F] = this

  /**
   * Allows the composition of [[Failure]]s. For example, when a failure is encountered,
   * the failure can be added to the existing [[Failure]] to provide a "stack trace".
   *
   * @param failure   The [[Failure]] to be added
   * @param composeFn The function specifying how the [[Failure]] is added
   * @tparam F1 The type of the new failure value
   * @return A [[Result]] with the composed failures
   */
  def compose[F1 >: F](failure: F1, composeFn: (F, F1) => F1): Result[S, F1]
end BaseFailure

final case class Failure[+S, +F](value: F) extends BaseFailure[S, F]:
  override def compose[F1 >: F](failure: F1, composeFn: (F, F1) => F1): Result[S, F1] = Failure(composeFn(value, failure))
end Failure

final case class ComposableFailure[+S](value: String, key: String = "error", values: Map[String, String] = Map())
  extends BaseFailure[S, Map[String, String]]:

  private val valueMap: Map[String, String] = if (key.isBlank) values else values ++ Map(key -> value)

  override def compose[F1 >: Map[String, String]](failure: F1, composeFn: (Map[String, String], F1) => F1): ComposableFailure[S] =
    ComposableFailure("<ignored>", "", valueMap ++ failure.asInstanceOf[Map[String, String]])

  def add(key: String, value: String): ComposableFailure[S] = ComposableFailure(value, key, valueMap)

  def messages: Map[String, String] = valueMap
end ComposableFailure

object Result:
  final case class FailureProjection[+S, +F](result: Result[S, F]):

    /**
     * Executes the specified side-effect function for [[Success]] results.
     * @param f The function mapping the success to a Unit
     * @tparam U
     */
    def foreach[U](f: F => U): Unit = result match
      case Failure(failure) => f(failure)
      case _ => ()

    def getOrElse[F1 >: F](or: => F1): F1 = result match
      case Failure(failure) => failure
      case _ => or

    def forall(predicate: F => Boolean): Boolean = result match
      case Failure(failure) => predicate(failure)
      case _ => true

    def exists(predicate: F => Boolean): Boolean = result match
      case Failure(failure) => predicate(failure)
      case _ => false

    def flatMap[S1, F1 >: F](f: F => Result[S1, F1]): Result[S1, F1] = result match
      case Failure(failure) => f(failure)
      case _ => result.asInstanceOf[Result[S1, F1]]

    def map[F1](f: F => F1): Result[S, F1] = result match
      case Failure(failure) => Failure(f(failure))
      case _ => result.asInstanceOf[Result[S, F1]]

    def filterToOption[S1](predicate: F => Boolean): Option[Result[S1, F]] = result match
      case x @ Failure(failure) if predicate(failure) => Some(x.asInstanceOf[Result[S1, F]])
      case _ => None

    def toOption: Option[F] = result match
      case Failure(failure) => Some(failure)
      case _ => None

end Result
