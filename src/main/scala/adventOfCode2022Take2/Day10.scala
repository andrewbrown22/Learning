package adventOfCode2022Take2

object Day10 extends App {

  val dayNineTestData = "addx 2\naddx 3\naddx 3\naddx -2\naddx 4\nnoop\naddx 1\naddx 4\naddx 1\nnoop\naddx 4\naddx 1\nnoop\naddx 2\naddx 5\naddx -28\naddx 30\nnoop\naddx 5\naddx 1\nnoop\naddx -38\nnoop\nnoop\nnoop\nnoop\naddx 5\naddx 5\naddx 3\naddx 2\naddx -2\naddx 2\nnoop\nnoop\naddx -2\naddx 12\nnoop\naddx 2\naddx 3\nnoop\naddx 2\naddx -31\naddx 32\naddx 7\nnoop\naddx -2\naddx -37\naddx 1\naddx 5\naddx 1\nnoop\naddx 31\naddx -25\naddx -10\naddx 13\nnoop\nnoop\naddx 18\naddx -11\naddx 3\nnoop\nnoop\naddx 1\naddx 4\naddx -32\naddx 15\naddx 24\naddx -2\nnoop\naddx -37\nnoop\nnoop\nnoop\naddx 5\naddx 5\naddx 21\naddx -20\nnoop\naddx 6\naddx 19\naddx -5\naddx -8\naddx -22\naddx 26\naddx -22\naddx 23\naddx 2\nnoop\nnoop\nnoop\naddx 8\naddx -10\naddx -27\naddx 33\naddx -27\nnoop\naddx 34\naddx -33\naddx 2\naddx 19\naddx -12\naddx 11\naddx -20\naddx 12\naddx 18\naddx -11\naddx -14\naddx 15\naddx 2\nnoop\naddx 3\naddx 2\nnoop\nnoop\nnoop\naddx -33\nnoop\naddx 1\naddx 2\nnoop\naddx 3\naddx 4\nnoop\naddx 1\naddx 2\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx 4\naddx -17\naddx 18\naddx 5\naddx -1\naddx 5\naddx 1\nnoop\nnoop\nnoop\nnoop"
    .split("\n").toList

  val newTestData: List[String] = dayNineTestData.map(addOrNoop => if (addOrNoop.contains("noop")) addOrNoop.replace("noop", "noop 0") else addOrNoop)
  val formattedNewData: List[(String, Int)] = newTestData.map(_.split(" ").toList).map{
    case List(a, b) => (a, b.toInt)
  }

  val dataThatsBeenFormatted: List[(String, Int)] = formattedNewData.map(_._1) zip operation(formattedNewData)

  def operation(listr: List[(String, Int)]) = {
    listr.scanLeft(1){(acc, curr) =>
      acc + curr._2
    }
  }

  def addTwoForAddX(list: List[(String, Int)]) = {
    list.scanLeft(0){(acc, curr) =>
      if (curr._1 == "addx") acc + 2
      else acc + 1
    }.drop(1)
  }

  val zipFinal: List[((String, Int), Int)] = dataThatsBeenFormatted zip addTwoForAddX(dataThatsBeenFormatted)

  def process(zippedList: List[((String, Int), Int)], firstInt: Int, secondInt: Int): List[((String, Int), Int)] ={

    zippedList.filter(x => x._2 == firstInt || x._2 == secondInt)
  }

  val twenty = process(zipFinal, 20, 21)
  val sixty = process(zipFinal, 60, 61)
  val hundo = process(zipFinal, 100, 101)
  val hundoForty = process(zipFinal, 140, 141)
  val hundoEighty = process(zipFinal, 180, 181)
  val twoHundoTwenty = process(zipFinal, 220, 221)

  def doTheMaths(list: List[((String, Int), Int)]): List[Int] = {
    if (list.head._2 % 2 == 0){
      list.map(x => x._1._2 * x._2)
    } else {
      list.map(x => x._1._2 * (x._2 - 1))
    }
  }

  def finalDef(): Int = {
    (doTheMaths(twenty) ::: doTheMaths(sixty) ::: doTheMaths(hundo) ::: doTheMaths(hundoForty) ::: doTheMaths(hundoEighty) ::: doTheMaths(twoHundoTwenty)).sum
  }

  println(finalDef())

}


