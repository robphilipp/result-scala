import akka.actor.Status.Failure

import scala.runtime.Nothing$

//trait Addable[V]:
//  def + (item: V): V

//sealed abstract class Result[S, F <: Addable[F]]:
sealed abstract class Result[S, F]:

  /**
   * Property that is `true` when the result is a success and `false` when the result is a failure
   *
   * @return `true` if the operation succeeded; `false` if the operation failed
   * @see failed
   */
  def succeeded: Boolean = this match
    case Success(_) => true
    case _ => false

  /**
   * Property that is `true` when the result is a failure and `false` when the result is a success
   *
   * @return `true` if the operation failed; `false` if the operation succeeded
   * @see succeeded
   */
  def failed: Boolean = !succeeded

  /**
   * Determines the equality of this result and the specified one. The results are considered equal if:
   * 1. They are both a success and their values are equal
   * 2. They are both a failure and their errors are equal
   *
   * @param result The result
   * @return `true` if the results are equal; `false` otherwise
   */
  def equals(result: Result[S, F]): Boolean = this match
    case Success(success) => result.value contains success
    case Failure(failure) => result.error contains failure
    case _ => false

  /**
   * Determines the equality of this result and the specified one. The results are considered equal if:
   * 1. They are both a success and their values are equal
   * 2. They are both a failure and their errors are equal
   *
   * @param result The result
   * @return `false` if the results are equal; `true` otherwise
   */
  def notEquals(result: Result[S, F]): Boolean = !equals(result)

  /**
   * Applies the specified `mapper` function to the success value of this result, and returns a new
   * [[Result]] that wraps the result of the `mapper`. When this result is a failure,
   * then it does **not** apply the `mapper`, but rather merely returns this result.
   *
   * @param mapper The mapper function that accepts the success value and returns a new value.
   * @return When this result is a success, then returns a [[Result]] that wraps the result
   *         of the `mapper` function. When this result is a failure, the returns this result.
   */
  def map[SP](mapper: S => SP): Result[SP, F] = this match
    case Success(success) => Success(mapper(success))
    case _ => this.asInstanceOf[Result[SP, F]]

  /**
   * Applies the specified `next` function to the success value of this result, and returns the
   * result of the `next` function. When this result is a failure, then it does **not** apply the
   * `next` function, but rather merely returns the failure result.
   *
   * @param fn The function to apply to this result's success value
   * @return When this result is a success, then returns the result of the `fn` function. When
   *         this result is a failure, then returns this result.
   */
  def flatMap[SP](fn: Result[S, F] => Result[SP, F]): Result[SP, F] = this match
    case Success(success) => fn(Success(success))
    case _ => this.asInstanceOf[Result[SP, F]]

  /**
   * Applies the filter to the success value of this result and returns the result if it matches
   * the filter predicate, or a failure if it doesn't match the predicate. When this result is a
   * failure, then returns that failure.
   *
   * @param filter The filter predicate
   * @return When this result is a success, then returns the success if it matches the predicate or
   *         a failure if it does not. When this result is a failure, then returns the failure.
   */
  def filter(filter: S => Boolean): Result[S, F] = this match
    case Success(success) => if filter(success) then this else this.asInstanceOf[Result[S, F]]
    case _ => this.asInstanceOf[Result[S, F]]
//  def filter(filter: S => Boolean, provider: () => F): Result[S, F] = this match
//    case Success(success) => if filter(success) then this else Failure(provider())
//    case _ => this

  /*
   * Accessor methods
   */

  protected def getValue: Option[S]

  protected def getError: Option[F]

  /**
   * The value of the operation the yielded this result.
   *
   * @return An [[Option]] holding the value of the operation that yielded a result; or
   *         [[None]] if the operation failed.
   */
  def value: Option[S] = this match
    case Success(_) => getValue
    case _ => None

  /**
   * Returns the value of the operation if this result is a success. If this result
   * is a failure, then returns the default value.
   *
   * @param default The value to return if this result is a failure
   * @return The value of the operation when this result is a success, or the default
   *         value when this result is a failure.
   */
  def valueOrDefault(default: S): S = this match
    case Success(_) => getValue.getOrElse(default)
    case _ => default

  /**
   * The failure value of the result.
   *
   * @return An [[Option]] holding the reason the operation failed; or [[None]]
   *         if the operation was a success.
   */
  def error: Option[F] = this match
    case Failure(_) => getError
    case _ => None

  /**
   * The failure value, if the operation resulted in a failure; or the
   * specified default value if the operation was a success
   *
   * @param default The default value for the failure
   * @return The failure value or the specified default
   */
  def errorOrDefault(default: F): F = this match
    case Failure(_) => getError.getOrElse(default)
    case _ => default


  /*
   | Methods for causing side effects
   */

  /**
   *
   * @param handler
   * @return
   */
  def onSuccess(handler: S => Unit): Result[S, F] = this match
    case Success(success) =>
      handler(success)
      this
    case _ => this

  def onFailure(handler: F => Unit): Result[S, F] = this match
    case Failure(failure) =>
      handler(failure)
      this
    case _ => this

  def always(handler: () => Unit): Result[S, F] =
    handler()
    this

end Result

case class Success[S, F](success: S) extends Result[S, F]:
  override def getValue: Option[S] = Some(success)

  override def getError: Option[F] = None

end Success

case class Failure[S, F](private val failure: F, private val updater: (F, Failure[S, F]) => Result[S, F]) extends Result[S, F]:
  override def getValue: Option[S] = None

  override def getError: Option[F] = Some(failure)

  def add(failure: F) = updater(failure, this)

end Failure

def identityUpdater[S, F](failure: F, current: Failure[S, F]) = current
def messageUpdater[S](updates: Map[String, String], current: Result[S, Map[String, String]]): Result[S, Map[String, String]] =
  Failure.addMessage(updates, current)

case object Failure:
  def apply[S, F](failure: F): Result[S, F] = new Failure(failure, identityUpdater)
  def apply[S, F](failure: F, updater: (F, Failure[S, F]) => Failure[S, F]): Result[S, F] = new Failure(failure, updater)

  def unapply[S, F](failureResult: Failure[S, F]): Option[F] = failureResult.getError

  def withMessages[S](reason: String): Result[S, Map[String, String]] = apply(Map("error" -> reason))

  def addMessage[S](
                     message: (String, String),
                     result: Result[S, Map[String, String]]
                   ): Result[S, Map[String, String]] = result match
    case Failure(failure) => apply(failure + message)
    case _ => apply(Map(message))

  def addMessage[S](
                     message: Map[String, String],
                     result: Result[S, Map[String, String]]
                   ): Failure[S, Map[String, String]] = result match
    case Failure(failure) => apply(failure ++ message).asInstanceOf[Failure[S, Map[String, String]]]
    case _ => apply(Map.newBuilder(message)).asInstanceOf[Failure[S, Map[String, String]]]

end Failure

// todo make a trait, WithMessages, and create a new Failure that uses the trait.

//case class Success[S](success: S) extends Result[S, Map[String, String]]:
//  override def getValue: Option[S] = Some(success)
//  override def getError: Option[Map[String, String]] = None
//
//case class Failure[S](reason: String, private val key: String = "error", private val messages: Map[String, String] = Map()) extends Result[S, Map[String, String]]:
//  override def getValue: Option[S] = None
//  override def getError: Option[Map[String, String]] = Some(messages + (key -> reason))
//  def add(key: String, message: String) = Failure(message, key, getError.get + (key -> message))



@main def test() =
  val suc = Success("yay!")
  println(suc.map(_.toUpperCase()))
  val res = Failure("this thing failed")
  println(res.map(_.toString.toUpperCase()))
  val res2 = Failure(Map("error" -> "this is the error"))
//  println(Failure("this is the error").add("more", "more error message"))
  println(Failure.addMessage(("second" -> "this is the second error"), res2))
  val res3 = Failure[String, Map[String, String]](Map("error" -> "error number 3"), messageUpdater[String])
    .add(Map("and another" -> "error to be added"))
