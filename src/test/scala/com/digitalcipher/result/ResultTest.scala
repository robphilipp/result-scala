package com.digitalcipher.result

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ResultTest extends AnyFlatSpec {

  "equals" should "be true when successes have the same value value" in {
    assert(Success(11) == Success(11))
    assert(Success("a") == Success("a"))
    assert(Success("a").withFailure[String] == Success("a"))
  }
  it should "be false when successes have different values" in {
    assert(Success("a") != Success("b"))
  }
  it should "be true when failures have the same value value" in {
    assert(Failure("damn") == Failure("damn"))
  }
  it should "be false when failures have different values" in {
    assert(Failure(10) != Failure(11))
  }
  it should "always be false when one result is a success and the other a failure" in {
    assert(Success(10) != Failure(10))
    assert(Failure(10) != Success(10))
  }

  "projection" should "perform operations on a failure" in {
    assert(Failure(10).projection.map(_ => 11) == Failure(11))
    assert(Failure(10).projection.filterToOption(x => x == 10).nonEmpty)
    assert(Failure(10).projection.filterToOption(x => x == 11).isEmpty)
  }
  it should "not perform operations on a success" in {
    assert(Success(10).projection.map(_ => 11) == Success(10))
  }

  "fold" should "replace the success value with the specified success supplier function" in {
    assert(Success(10).fold(_ => "yay", _ => "boo") == "yay")
    assert(Success(10).withFailure[String].fold(_ => "yay", _ => "boo") == "yay")
  }
  it should "replace the failure value with the specified failure supplier function" in {
    assert(Success(10).fold(_ => "yay", _ => "boo") == "yay")
  }

  "swap" should "swap a Success for a Failure" in {
    assert(Success(10).swap == Failure(10))
  }
  it should "swap a Failure for a Success" in {
    assert(Failure(10).swap == Success(10))
  }

  "foreach" should "perform the side-effect on a Success" in {
    var test = ""
    Success("yay").foreach(x => test = x.toUpperCase())
    assert(test == "YAY")
  }
  it should "not perform the side-effect on a Failure" in {
    var test = "yay"
    Failure("boo").withSuccess[String].foreach(x => test = x.toUpperCase())
    assert(test == "yay")
  }

  "getOrElse" should "return the success value on success or the value from the supplier for a failure" in {
    assert(Success("yay").getOrElse( "boo") == "yay")
  }
  it should "return the value from the supplier for a failure" in {
    assert(Failure("yay").getOrElse( "boo") == "boo")
  }

  "orElse" should "return the success value on success, or a supplied result on failure" in {
    assert(Success("this one").orElse(Success("that one")) == Success("this one"))
    assert(Success("this one").orElse(Failure("that one")) == Success("this one"))
  }
  it should "return the supplied result on failure" in {
    assert(Failure("this one").orElse(Success("that one")) == Success("that one"))
    assert(Failure("this one").orElse(Failure("that one")) == Failure("that one"))
  }

  "contains" should "contain the success value on success" in {
    assert(Success("a member").contains("a member"))
  }
  it should "not contain a value that is not the success value on success" in {
    assert(!Success("a member").contains("not a member"))
  }
  it should "not contain the success value on failure" in {
    assert(!Failure("a member").contains("a member"))
  }

  "forall" should "be true when the specified predicate evaluates to true on the success value" in {
    assert(Success(10).forall(x => x % 2 == 0))
  }
  it should "be false when the specified predicate evaluates to false on the success value" in {
    assert(!Success(11).forall(x => x % 2 == 0))
  }
  it should "be true if the result is a failure" in {
    assert(Failure(10).forall(x => x == 11))
  }

  "exists" should "be true when the specified predicate evaluates to true on the success value" in {
    assert(Success(10).exists(x => x % 2 == 0))
  }
  it should "be false when the specified predicate evaluates to false on the success value" in {
    assert(!Success(11).exists(x => x % 2 == 0))
  }
  it should "be false if the result is a failure" in {
    assert(!Failure(10).exists(x => x == 10))
  }

  "flatMap" should "return a transformed result on success" in {
    assert(Success(10).flatMap(x => Success(x * math.Pi)).getOrElse(0) == 10 * math.Pi)
  }
  it should "return an untransformed result on a failure" in {
    assert(Failure[Double, Int](10).flatMap(x => Success(x * math.Pi)) == Failure(10))
  }

  "flatten" should "return the inner result on an outer success" in {
    assert(Success(Success(10)).flatten == Success(10))
    assert(Success(Failure(10)).flatten == Failure(10))
    assert(Success(Success(Failure(10))).flatten.flatten == Failure(10))
  }
  it should "return the outer result on an outer failure" in {
    assert(Failure(Success(10)).flatten == Failure(Success(10)))
    assert(Success(Failure(Success(10))).flatten == Failure(Success(10)))
    assert(Failure(Failure(10)).flatten == Failure(Failure(10)))
  }

  "map" should "transform the value of success results" in {
    assert(Success("Yay").map(_.toLowerCase()) == Success("yay"))
  }
  it should "not transform the value of failure results" in {
    assert(Failure[String, String]("oh shit").map(_.toUpperCase()) == Failure("oh shit"))
    assert(Failure[Int, String]("oops").map(_ * 10).getOrElse("bummer") == "bummer")
  }

  "filterOrElse" should "return a success result if the result is a success and the value meets the predicate" in {
    assert(Success("this is not a sentence").filterOrElse(_.length > 5, "oops") == Success("this is not a sentence"))
  }
  it should "return the default value if the result is a success and the value does not meet the predicate" in {
    assert(Success("this is not a sentence").filterOrElse(_.length < 5, "oops") == Failure("oops"))
  }
  it should "return the un modified failure when the result is a failure" in {
    assert(
      Failure[String, String]("son, you're a failure")
        .filterOrElse(_.length > 10, "nope, wont happen") == Failure("son, you're a failure")
    )
  }

  "toOption" should "return a Some when the result is a success" in {
    assert(Success("am i optional").toOption.map(_ => "no, you are needed").contains("no, you are needed"))
  }
  it should "return a None when the result is a failure" in {
    assert(Failure("am i really a failure").toOption.isEmpty)
  }

  "toTry" should "convert a Success to a scala.util.Success with the same value" in {
    assert(Success(10).toTry == scala.util.Success(10))
  }
  it should "convert a Failure to a scala.util.Failure with the same value" in {
    assert(Failure(Throwable("file not found")).toTry.isFailure)
    assert(Failure(Throwable("file not found")).toTry.failed.get.getMessage == "file not found")
  }

  "toEither" should "convert a Success into a Right" in {
    assert(Success(10).toEither.contains(10))
  }
  it should "convert a Failure into a Left" in {
    assert(Failure(314).toEither == Left(314))
  }

  "isSuccess" should "be true when the result is a success" in {
    assert(Success(10).isSuccess)
  }
  it should "be false when the result is a failure" in {
    assert(!Failure(12).isSuccess)
  }

  "isFailure" should "be true when the result is a failure" in {
    assert(Failure(10).isFailure)
  }
  it should "be false when the result is a success" in {
    assert(!Success(12).isFailure)
  }

  "compose" should "add a failure to the existing failure" in {
    assert(
      Failure[String, Map[String, String]](Map("error" -> "first message"))
        .compose(Map("error2" -> "second message"), (m1, m2) => m1 ++ m2)
        .projection
        .getOrElse(Map()) == Map("error" -> "first message", "error2" -> "second message")
    )
  }
  it should "add a failure to an existing composable failure with the compose function" in {
    ComposableFailure[Double]("this is a failure with default error key")
      .compose(Map("error2" -> "this is the second error"), (m1, m2) => m1 ++ m2)
      .messages == Map("error" -> "this is a failure with default error key", "error2" -> "this is the second error")

  }
  "add" should "add a failure to an existing composable failure" in {
    assert(
      ComposableFailure[Double]("this is a failure with default error key")
        .add("error2", "this is the second error")
        .messages == Map("error" -> "this is a failure with default error key", "error2" -> "this is the second error")
    )
  }
}
