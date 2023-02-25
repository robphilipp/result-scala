package com.digitalcipher.result

import scala.util.Try

sealed abstract class Result[+S, +F] extends Serializable:

  /**
   * Returns a failure projection that allows manipulation of the [[Failure]]
   * with modifying the [[Success]] value. The [[Result]] class is success-biased,
   * so that operations on a [[Success]] leave [[Failure]]s unmodified. The
   * projection does the opposite: it allows operations on the [[Failure]], leaving
   * the [[Success]] unmodified.
   * @return A [[Result.FailureProjection]]
   */
  def projection = Result.FailureProjection(this)

  /**
   * Applies the success function `successFn` when this is a success.
   * When this is a failure, calls the `failureFn`. Both functions
   * map the value to a combined type, `C`.
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
   * value as the [[Failure]]
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
    case Success(success) => effectFn(success)
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

  def toTry(implicit ev: F <:< Throwable): Try[S] = this match
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
   * Upcasts the `Success[S, F]` to a `Result[S, F1]`
   * {{{
   *   Success("x")                  // Result[String, Nothing]
   *   Success("x").withFailure[Int] // Result[String, Int]
   * }}}
   *
   * @tparam F1
   * @return
   */
  def withFailure[F1 >: F]: Result[S, F1] = this

end Success

final case class Failure[+S, +F](value: F) extends Result[S, F]:
  override def isSuccess: Boolean = false

  override def isFailure: Boolean = true

  /**
   * Upcasts the `Failure[S, F]` to `Result[S1, F]`.
   * {{{
   *   Failure("x")                  // Result[Nothing, String]
   *   Failure("x").withSuccess[Int] // Result[Int, String]
   * }}}
   *
   * @tparam S1
   * @return
   */
  def withSuccess[S1 >: S]: Result[S1, F] = this
end Failure

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

//sealed abstract class Result[S, F <: Addable[F]]:
//sealed class Result[S](
//                        private val value: Option[S],
//                        val messages: Map[String, String] = Map(),
//                        val status: Status
//                      ):
//  def orElse(other: S): S = value match
//    case Some(thing) => thing
//    case None => other
//
//  //  def orElseNothing() = orElse(Nothing)
//
//  def orElseGet(supplier: () => S): S = value match
//    case Some(thing) => thing
//    case None => supplier()
//
//  def orElseGet(resultFun: Result[S] => S): S = value match
//    case Some(thing) => thing
//    case None => resultFun(this)
//
//  /**
//   * Property that is `true` when the result is a success and `false` when the result is a failure
//   *
//   * @return `true` if the operation succeeded; `false` if the operation failed
//   * @see failed
//   */
//  def isSuccess: Boolean = value.nonEmpty
//
//  /**
//   * Property that is `true` when the result is a failure and `false` when the result is a success
//   *
//   * @return `true` if the operation failed; `false` if the operation succeeded
//   * @see succeeded
//   */
//  def isNotSuccess: Boolean = !isSuccess
//
//  def message(error: String): Option[String] = messages.get(error)
//
////  def filter(predicate: S => S): Result[S] = value match
////    case Some(thing) => success(predicate(thing))
////    case None => this
//
//  def map(mapping: S => S): Result[S] = value match
//    case Some(thing) => success(mapping(thing))
//    case None => this
//
//  /**
//   * Determines the equality of this result and the specified one. The results are considered equal if:
//   * 1. They are both a success and their values are equal
//   * 2. They are both a failure and their errors are equal
//   *
//   * @param that The result
//   * @return `true` if the results are equal; `false` otherwise
//   */
//  override def equals(that: Any): Boolean = that match
//    case result: Result[S] => value match
//      case Some(thing) => result.value contains thing
//      case None => result.messages == messages
//
//    case _ => false
////    case Some(thing) => result.value contains thing
////    case None => result.messages == messages
//
//  //  /**
//  //   * Determines the equality of this result and the specified one. The results are considered equal if:
//  //   * 1. They are both a success and their values are equal
//  //   * 2. They are both a failure and their errors are equal
//  //   *
//  //   * @param result The result
//  //   * @return `false` if the results are equal; `true` otherwise
//  //   */
//  //  def notEquals(result: Result[S, F]): Boolean = !equals(result)
//  //
//  //  /**
//  //   * Applies the specified `mapper` function to the success value of this result, and returns a new
//  //   * [[Result]] that wraps the result of the `mapper`. When this result is a failure,
//  //   * then it does **not** apply the `mapper`, but rather merely returns this result.
//  //   *
//  //   * @param mapper The mapper function that accepts the success value and returns a new value.
//  //   * @return When this result is a success, then returns a [[Result]] that wraps the result
//  //   *         of the `mapper` function. When this result is a failure, the returns this result.
//  //   */
//  //  def map[SP](mapper: S => SP): Result[SP, F] = this match
//  //    case Success(success) => Success(mapper(success))
//  //    case _ => this.asInstanceOf[Result[SP, F]]
//  //
//  //  /**
//  //   * Applies the specified `next` function to the success value of this result, and returns the
//  //   * result of the `next` function. When this result is a failure, then it does **not** apply the
//  //   * `next` function, but rather merely returns the failure result.
//  //   *
//  //   * @param fn The function to apply to this result's success value
//  //   * @return When this result is a success, then returns the result of the `fn` function. When
//  //   *         this result is a failure, then returns this result.
//  //   */
//  //  def flatMap[SP](fn: Result[S, F] => Result[SP, F]): Result[SP, F] = this match
//  //    case Success(success) => fn(Success(success))
//  //    case _ => this.asInstanceOf[Result[SP, F]]
//  //
//  //  /**
//  //   * Applies the filter to the success value of this result and returns the result if it matches
//  //   * the filter predicate, or a failure if it doesn't match the predicate. When this result is a
//  //   * failure, then returns that failure.
//  //   *
//  //   * @param filter The filter predicate
//  //   * @return When this result is a success, then returns the success if it matches the predicate or
//  //   *         a failure if it does not. When this result is a failure, then returns the failure.
//  //   */
//  //  def filter(filter: S => Boolean): Result[S, F] = this match
//  //    case Success(success) => if filter(success) then this else this.asInstanceOf[Result[S, F]]
//  //    case _ => this.asInstanceOf[Result[S, F]]
//  ////  def filter(filter: S => Boolean, provider: () => F): Result[S, F] = this match
//  ////    case Success(success) => if filter(success) then this else Failure(provider())
//  ////    case _ => this
//  //
//  //  /*
//  //   * Accessor methods
//  //   */
//  //
//  //  protected def getValue: Option[S]
//  //
//  //  protected def getError: Option[F]
//  //
//  //  /**
//  //   * The value of the operation the yielded this result.
//  //   *
//  //   * @return An [[Option]] holding the value of the operation that yielded a result; or
//  //   *         [[None]] if the operation failed.
//  //   */
//  ////  def value: Option[S] = this match
//  ////    case Success(_) => getValue
//  ////    case _ => None
//  //
//  //  /**
//  //   * Returns the value of the operation if this result is a success. If this result
//  //   * is a failure, then returns the default value.
//  //   *
//  //   * @param default The value to return if this result is a failure
//  //   * @return The value of the operation when this result is a success, or the default
//  //   *         value when this result is a failure.
//  //   */
//  //  def valueOrDefault(default: S): S = this match
//  //    case Success(_) => getValue.getOrElse(default)
//  //    case _ => default
//  //
//  //  /**
//  //   * The failure value of the result.
//  //   *
//  //   * @return An [[Option]] holding the reason the operation failed; or [[None]]
//  //   *         if the operation was a success.
//  //   */
//  //  def error: Option[F] = this match
//  //    case Failure(_) => getError
//  //    case _ => None
//  //
//  //  /**
//  //   * The failure value, if the operation resulted in a failure; or the
//  //   * specified default value if the operation was a success
//  //   *
//  //   * @param default The default value for the failure
//  //   * @return The failure value or the specified default
//  //   */
//  //  def errorOrDefault(default: F): F = this match
//  //    case Failure(_) => getError.getOrElse(default)
//  //    case _ => default
//  //
//  //
//  //  /*
//  //   | Methods for causing side effects
//  //   */
//  //
//  //  /**
//  //   *
//  //   * @param handler
//  //   * @return
//  //   */
//  //  def onSuccess(handler: S => Unit): Result[S, F] = this match
//  //    case Success(success) =>
//  //      handler(success)
//  //      this
//  //    case _ => this
//  //
//  //  def onFailure(handler: F => Unit): Result[S, F] = this match
//  //    case Failure(failure) =>
//  //      handler(failure)
//  //      this
//  //    case _ => this
//  //
//  //  def always(handler: () => Unit): Result[S, F] =
//  //    handler()
//  //    this
//
//end Result
//
//object Result:
//
//  def of[S](value: S): Result[S] = success(value)
//
//  def success[S](value: S): Result[S] = Result(Some(value), Map(), Success)
//
//  def failure[S](message: String) = Failed("error", message)
//
//class Failed[S](
//                 val name: String,
//                 val message: String,
//                 override val messages: Map[String, String] = Map()
//               )
//  extends Result[S](None, messages + (name -> message), Failure):
//
//  def add(name: String, message: String) = Failed(name, message, messages)
//
//sealed trait Status
//
//case object Success extends Status
//
//case object NotFound extends Status
//
//case object BadRequest extends Status
//
//case object Failure extends Status
