package com.digitalcipher.result

import Result.success

//sealed abstract class Result[S, F <: Addable[F]]:
sealed class Result[S](
                        private val value: Option[S],
                        val messages: Map[String, String] = Map(),
                        val status: Status
                      ):
  def orElse(other: S): S = value match
    case Some(thing) => thing
    case None => other

  //  def orElseNothing() = orElse(Nothing)

  def orElseGet(supplier: () => S): S = value match
    case Some(thing) => thing
    case None => supplier()

  def orElseGet(resultFun: Result[S] => S): S = value match
    case Some(thing) => thing
    case None => resultFun(this)

  /**
   * Property that is `true` when the result is a success and `false` when the result is a failure
   *
   * @return `true` if the operation succeeded; `false` if the operation failed
   * @see failed
   */
  def isSuccess: Boolean = value.nonEmpty

  /**
   * Property that is `true` when the result is a failure and `false` when the result is a success
   *
   * @return `true` if the operation failed; `false` if the operation succeeded
   * @see succeeded
   */
  def isNotSuccess: Boolean = !isSuccess

  def message(error: String): Option[String] = messages.get(error)

//  def filter(predicate: S => S): Result[S] = value match
//    case Some(thing) => success(predicate(thing))
//    case None => this

  def map(mapping: S => S): Result[S] = value match
    case Some(thing) => success(mapping(thing))
    case None => this

  /**
   * Determines the equality of this result and the specified one. The results are considered equal if:
   * 1. They are both a success and their values are equal
   * 2. They are both a failure and their errors are equal
   *
   * @param that The result
   * @return `true` if the results are equal; `false` otherwise
   */
  override def equals(that: Any): Boolean = that match
    case result: Result[S] => value match
      case Some(thing) => result.value contains thing
      case None => result.messages == messages

    case _ => false
//    case Some(thing) => result.value contains thing
//    case None => result.messages == messages

  //  /**
  //   * Determines the equality of this result and the specified one. The results are considered equal if:
  //   * 1. They are both a success and their values are equal
  //   * 2. They are both a failure and their errors are equal
  //   *
  //   * @param result The result
  //   * @return `false` if the results are equal; `true` otherwise
  //   */
  //  def notEquals(result: Result[S, F]): Boolean = !equals(result)
  //
  //  /**
  //   * Applies the specified `mapper` function to the success value of this result, and returns a new
  //   * [[Result]] that wraps the result of the `mapper`. When this result is a failure,
  //   * then it does **not** apply the `mapper`, but rather merely returns this result.
  //   *
  //   * @param mapper The mapper function that accepts the success value and returns a new value.
  //   * @return When this result is a success, then returns a [[Result]] that wraps the result
  //   *         of the `mapper` function. When this result is a failure, the returns this result.
  //   */
  //  def map[SP](mapper: S => SP): Result[SP, F] = this match
  //    case Success(success) => Success(mapper(success))
  //    case _ => this.asInstanceOf[Result[SP, F]]
  //
  //  /**
  //   * Applies the specified `next` function to the success value of this result, and returns the
  //   * result of the `next` function. When this result is a failure, then it does **not** apply the
  //   * `next` function, but rather merely returns the failure result.
  //   *
  //   * @param fn The function to apply to this result's success value
  //   * @return When this result is a success, then returns the result of the `fn` function. When
  //   *         this result is a failure, then returns this result.
  //   */
  //  def flatMap[SP](fn: Result[S, F] => Result[SP, F]): Result[SP, F] = this match
  //    case Success(success) => fn(Success(success))
  //    case _ => this.asInstanceOf[Result[SP, F]]
  //
  //  /**
  //   * Applies the filter to the success value of this result and returns the result if it matches
  //   * the filter predicate, or a failure if it doesn't match the predicate. When this result is a
  //   * failure, then returns that failure.
  //   *
  //   * @param filter The filter predicate
  //   * @return When this result is a success, then returns the success if it matches the predicate or
  //   *         a failure if it does not. When this result is a failure, then returns the failure.
  //   */
  //  def filter(filter: S => Boolean): Result[S, F] = this match
  //    case Success(success) => if filter(success) then this else this.asInstanceOf[Result[S, F]]
  //    case _ => this.asInstanceOf[Result[S, F]]
  ////  def filter(filter: S => Boolean, provider: () => F): Result[S, F] = this match
  ////    case Success(success) => if filter(success) then this else Failure(provider())
  ////    case _ => this
  //
  //  /*
  //   * Accessor methods
  //   */
  //
  //  protected def getValue: Option[S]
  //
  //  protected def getError: Option[F]
  //
  //  /**
  //   * The value of the operation the yielded this result.
  //   *
  //   * @return An [[Option]] holding the value of the operation that yielded a result; or
  //   *         [[None]] if the operation failed.
  //   */
  ////  def value: Option[S] = this match
  ////    case Success(_) => getValue
  ////    case _ => None
  //
  //  /**
  //   * Returns the value of the operation if this result is a success. If this result
  //   * is a failure, then returns the default value.
  //   *
  //   * @param default The value to return if this result is a failure
  //   * @return The value of the operation when this result is a success, or the default
  //   *         value when this result is a failure.
  //   */
  //  def valueOrDefault(default: S): S = this match
  //    case Success(_) => getValue.getOrElse(default)
  //    case _ => default
  //
  //  /**
  //   * The failure value of the result.
  //   *
  //   * @return An [[Option]] holding the reason the operation failed; or [[None]]
  //   *         if the operation was a success.
  //   */
  //  def error: Option[F] = this match
  //    case Failure(_) => getError
  //    case _ => None
  //
  //  /**
  //   * The failure value, if the operation resulted in a failure; or the
  //   * specified default value if the operation was a success
  //   *
  //   * @param default The default value for the failure
  //   * @return The failure value or the specified default
  //   */
  //  def errorOrDefault(default: F): F = this match
  //    case Failure(_) => getError.getOrElse(default)
  //    case _ => default
  //
  //
  //  /*
  //   | Methods for causing side effects
  //   */
  //
  //  /**
  //   *
  //   * @param handler
  //   * @return
  //   */
  //  def onSuccess(handler: S => Unit): Result[S, F] = this match
  //    case Success(success) =>
  //      handler(success)
  //      this
  //    case _ => this
  //
  //  def onFailure(handler: F => Unit): Result[S, F] = this match
  //    case Failure(failure) =>
  //      handler(failure)
  //      this
  //    case _ => this
  //
  //  def always(handler: () => Unit): Result[S, F] =
  //    handler()
  //    this

end Result

object Result:

  def of[S](value: S): Result[S] = success(value)

  def success[S](value: S): Result[S] = Result(Some(value), Map(), Success)

  def failure[S](message: String) = Failed("error", message)

class Failed[S](
                 val name: String,
                 val message: String,
                 override val messages: Map[String, String] = Map()
               )
  extends Result[S](None, messages + (name -> message), Failure):

  def add(name: String, message: String) = Failed(name, message, messages)

sealed trait Status

case object Success extends Status

case object NotFound extends Status

case object BadRequest extends Status

case object Failure extends Status
