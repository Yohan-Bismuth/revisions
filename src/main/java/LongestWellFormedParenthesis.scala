import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable

/*

Let S be the set of strings containing only the characters '(' and ')'.
Let's define A, a subset of S, such that :
The empty string is in A
if s is in A, then "(s)" is in A.
If s1 and s2 are in A then "s1s2" is in A.
Can you write a function that takes a string s belonging to S as input, and prints the length of the longest substring of s that belongs to A ?

Examples:

-"" is well formed
-"(())" is well formed
-"()()" is well formed
-")(" is not well formed
 */


object LongestWellFormedParenthesis extends App {

  // First naive
  def longestSubstring(str: String): Int = {
    val substrings: List[String] = getAllSubstrings(str)
    substrings.map(ss => ss.length).max
  }

  def isWellFormed(str: String): Boolean = {
    if (str.length == 0)
      true
    else {
      var indent = 0
      for (c <- str.toCharArray) {
        if (c == '(')
          indent += 1
        if (c == ')')
          indent -= 1
        if (indent < 0)
          return false
      }
      indent == 0
    }
  }

  def getAllSubstrings(str: String): List[String] = {
    var acc = List[String]()
    for (i <- 0 to str.length) {
      for (j <- i + 1 to str.length)
        acc ++= List(str.slice(i, j))
    }
    acc
  }

  //Then smart
  import scala.collection.mutable.Map

  case class Substring(length: Int, indent: Int)

  val resMap : Map[Int, Substring] = Map()

  def longestSubstringSmart(str: String): Int = {
    val carr = str.toCharArray
    var maxLength = 0
    val lastPosition = mutable.Stack[Int]()

    for (i <- 0 until carr.length) {

      if (carr(i) == '(' ){
        lastPosition.push(i)
      }

      if (carr(i) == ')') {
        if (lastPosition.nonEmpty) {
          val candidateLength = i + 1 - lastPosition.pop
          maxLength = List(maxLength, candidateLength).max
        } else {
          maxLength = List(maxLength, 0).max
        }
      }
    }
    maxLength
  }

  def longestSubstringSkype(s: String): Int = {
    var longest = 0
    var current = 0
    var opened = 0 // non negative by design

    for (_ <- s.toCharArray) { c:Char =>
      (c, opened) match {
        case ('(', _) => // still well formed
          opened += 1
          current += 1
        case (')', 0) => // finishing well formed part
          longest = Math.max(longest, current)
          current = 0
        case (')', _) => // close well formed bracket
          opened -= 1
          current += 1
          if (opened == 0)
            longest = Math.max(longest, current)
      }
    }
    longest
  }

  println(longestSubstringSmart(")("))
  println(longestSubstringSmart("())(())"))
  println(longestSubstringSkype("())(())"))
  println(longestSubstringSmart("()()"))
}