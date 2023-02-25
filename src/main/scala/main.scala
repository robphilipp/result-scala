import com.digitalcipher.result.Success
//import akka.actor.Status.Failure
//
//import scala.util.control
//
////trait Addable[V]:
////  def + (item: V): V
//
//
//
////case class Success[S, F](success: S) extends Result[S, F]:
////  override def getValue: Option[S] = Some(success)
////
////  override def getError: Option[F] = None
////
////end Success
////
////case class Failure[S, F](private val failure: F, private val updater: (F, Failure[S, F]) => Result[S, F]) extends Result[S, F]:
////  override def getValue: Option[S] = None
////
////  override def getError: Option[F] = Some(failure)
////
////  def add(failure: F) = updater(failure, this)
////
////end Failure
////
////def identityUpdater[S, F](failure: F, current: Failure[S, F]) = current
////def messageUpdater[S](updates: Map[String, String], current: Result[S, Map[String, String]]): Result[S, Map[String, String]] =
////  Failure.addMessage(updates, current)
////
////case object Failure:
////  def apply[S, F](failure: F): Result[S, F] = new Failure(failure, identityUpdater)
////  def apply[S, F](failure: F, updater: (F, Failure[S, F]) => Failure[S, F]): Result[S, F] = new Failure(failure, updater)
////
////  def unapply[S, F](failureResult: Failure[S, F]): Option[F] = failureResult.getError
////
////  def withMessages[S](reason: String): Result[S, Map[String, String]] = apply(Map("error" -> reason))
////
////  def addMessage[S](
////                     message: (String, String),
////                     result: Result[S, Map[String, String]]
////                   ): Result[S, Map[String, String]] = result match
////    case Failure(failure) => apply(failure + message)
////    case _ => apply(Map(message))
////
////  def addMessage[S](
////                     message: Map[String, String],
////                     result: Result[S, Map[String, String]]
////                   ): Failure[S, Map[String, String]] = result match
////    case Failure(failure) => apply(failure ++ message).asInstanceOf[Failure[S, Map[String, String]]]
////    case _ => apply(Map.newBuilder(message)).asInstanceOf[Failure[S, Map[String, String]]]
////
////end Failure
//
//
//
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
  println("yay")
  Success(10)
//  success(10).map()
//  val suc = Success("yay!")
//  println(suc.map(_.toUpperCase()))
//  val res = Failure("this thing failed")
//  println(res.map(_.toString.toUpperCase()))
//  val res2 = Failure(Map("error" -> "this is the error"))
////  println(Failure("this is the error").add("more", "more error message"))
//  println(Failure.addMessage(("second" -> "this is the second error"), res2))
//  val res3 = Failure[String, Map[String, String]](Map("error" -> "error number 3"), messageUpdater[String])
//    .add(Map("and another" -> "error to be added"))
