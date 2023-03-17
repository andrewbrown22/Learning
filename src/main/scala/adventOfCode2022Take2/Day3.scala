package adventOfCode2022Take2

object Day3 extends App {

  def rucksack(input: String) = {
    val inputData = input.split("\n").toList

    val x: List[List[String]] = inputData.sliding(3,3).toList

    val headOfList = x.map(_.head)
    val headOfTail = x.map(_.tail.head)
    val lastinList = x.map(_.last)

    val zippedLists: List[((String, String), String)] = headOfList zip headOfTail zip lastinList

    val firstZippedList: List[String] = zippedLists.map(x => x._1._1 filter(y => x._1._2.contains(y)))
    val secondZippedList: List[String] = zippedLists.map(x => x._2 filter(y => x._1._1.contains(y)))

    val finalSecondList = secondZippedList.map(_.distinct).map(x => replaceAllLettersInMap(x))
    val finalFirstList = firstZippedList.map(_.distinct).map(x => replaceAllLettersInMap(x))

    val finalList: List[(List[Int], List[Int])] = finalFirstList zip finalSecondList

    finalList.flatMap(x => x._1 filter(y => x._2.contains(y))).sum

  }

  def replaceAllLettersInMap(string: String): List[Int] = {
    string.map {
      case 'a' => 1
      case 'b' => 2
      case 'c' => 3
      case 'd' => 4
      case 'e' => 5
      case 'f' => 6
      case 'g' => 7
      case 'h' => 8
      case 'i' => 9
      case 'j' => 10
      case 'k' => 11
      case 'l' => 12
      case 'm' => 13
      case 'n' => 14
      case 'o' => 15
      case 'p' => 16
      case 'q' => 17
      case 'r' => 18
      case 's' => 19
      case 't' => 20
      case 'u' => 21
      case 'v' => 22
      case 'w' => 23
      case 'x' => 24
      case 'y' => 25
      case 'z' => 26
      case 'A' => 1 + 26
      case 'B' => 2 + 26
      case 'C' => 3 + 26
      case 'D' => 4 + 26
      case 'E' => 5 + 26
      case 'F' => 6 + 26
      case 'G' => 7 + 26
      case 'H' => 8 + 26
      case 'I' => 9 + 26
      case 'J' => 10 + 26
      case 'K' => 11 + 26
      case 'L' => 12 + 26
      case 'M' => 13 + 26
      case 'N' => 14 + 26
      case 'O' => 15 + 26
      case 'P' => 16 + 26
      case 'Q' => 17 + 26
      case 'R' => 18 + 26
      case 'S' => 19 + 26
      case 'T' => 20 + 26
      case 'U' => 21 + 26
      case 'V' => 22 + 26
      case 'W' => 23 + 26
      case 'X' => 24 + 26
      case 'Y' => 25 + 26
      case 'Z' => 26 + 26
    }.toList
  }

  rucksack("dataGoesHere")
}
