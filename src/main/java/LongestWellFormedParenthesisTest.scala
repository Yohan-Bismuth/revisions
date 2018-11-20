import org.scalatest.{Matchers, WordSpec}

class LongestWellFormedParenthesisTest extends WordSpec with Matchers {

  import LongestWellFormedParenthesis._

  "LongestWellFormedParenthesis" should {



    "Check empty string is well formed" in {
      isWellFormed("") shouldBe true
    }

    "Check recursive well formed is well formed" in {
      isWellFormed("(())") shouldBe true
    }

    "Check unclosed well formed is well formed" in {
      isWellFormed("(()") shouldBe false
    }

    "Check concat string is well formed" in {
      isWellFormed("()()") shouldBe true
    }

    "Check not opened string is not well formed" in {
      isWellFormed(")(") shouldBe false
    }

    "get All Substrings" in {
      getAllSubstrings("(())") shouldEqual List("(", "((", "(()", "(())", "(", "()", "())", ")", "))", ")")
    }

    "Global test 1" in {
      longestSubstring("((((()))))") shouldEqual 10
    }

    "Global test 2" in {
      longestSubstringSmart("((((()))))") shouldEqual 10
    }

    "Global test 3" in {
      longestSubstringSmart("(()(((((") shouldEqual 2
    }
  }

}