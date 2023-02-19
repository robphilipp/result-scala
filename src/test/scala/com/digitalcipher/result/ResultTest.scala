package com.digitalcipher.result

import com.digitalcipher.result.Result.success
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ResultTest extends AnyFlatSpec {

  "A Result" should "equal itself" in {
    val result = success(10)
    assert(result == result)
  }

  "A Result" should "equal a Result with the same value" in {
    assert(success(10) == success(10))
  }
}
