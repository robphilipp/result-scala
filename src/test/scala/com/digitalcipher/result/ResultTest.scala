package com.digitalcipher.result

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ResultTest extends AnyFlatSpec {

  "equals" should "be true when successes have the same value value" in {
    assert(Success(11) == Success(11))
    assert(Success("a") == Success("a"))
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
    Failure[String, String]("boo").foreach(x => test = x.toUpperCase())
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
    assert(Failure(Failure(10)) == Failure(Failure(10)))
  }
}
