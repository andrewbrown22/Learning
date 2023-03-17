package adventOfCode2022Take2

object Day2 extends App {

  def rockPaperScissors(input: String) = {
    val RPS = input.split("\n").sliding(1).toList.map(_.toList.flatMap(_.split(" ")))

    foldDatLeft(RPS)
  }

  def lostFunction(input: String) = {
    input match {
      case "A" => 3
      case "B" => 1
      case "C" => 2
    }
  }

  def drawFunction(findOutWhatsTail: String): Int = {
    findOutWhatsTail match {
      case "A" => 4
      case "B" => 5
      case "C" => 6
    }
  }

  def winFunction(input: String): Int = {
    input match {
      case "A" => 8
      case "B" => 9
      case "C" => 7
    }
  }

  def caseMatchyTwo(matchy: List[String]) = {
    if (matchy.tail.head == "Y") {
      drawFunction(matchy.head)
    } else if (matchy.tail.head == "X") {
      lostFunction(matchy.head)
    } else {
      winFunction(matchy.head)
    }

  }

  def foldDatLeft(input: List[List[String]]) = {
    input.foldLeft(0) {
      (acc, matchy) => acc + caseMatchyTwo(matchy)
    }
  }

  rockPaperScissors("dataGoesHere")
}
