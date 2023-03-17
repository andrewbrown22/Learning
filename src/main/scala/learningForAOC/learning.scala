package learningForAOC

object learning extends App {

  val listOfNums = List(1,2,3,4,5,4,8,1,3,4,6,8,2,10,3)

  val listOLetters = List("a","b","c","d","a","a","a","a")

  val mulitplyFoldLeft = listOfNums.foldLeft(0)(_ * _)
  println(mulitplyFoldLeft)

  def turnLettersIntoInts(input: String): Int = {
    input match {
      case "a" => 1
      case "b" => 2
      case "c" => 3
      case "d" => 4
        }
  }

  val foldLettersLeft = listOLetters.foldLeft(0){
    (acc, letter) => acc + turnLettersIntoInts(letter)
  }

  println(foldLettersLeft)

  case class Person(name: String, gender: String)

  val persons = List(Person("Thomas", "male"), Person("Sowell", "male"), Person("Liz", "female"))

  val foldPerson = persons.foldLeft(List[String]()) { (acc, cheese) =>
    val title = cheese.gender match {
      case "male" => "mr"
      case "female" => "miss"
    }
    acc :+ s"$title ${cheese.name}"
  }

  println(foldPerson)

  class Rational(x: Int, y: Int) {
    def numer = x
    def denom = y
  }

  new Rational(10, 20)

  val xs = List(1,2,3,4)

  var i = 0
  var sum = 0
  while (i < xs.length) {
    val x = xs(i)
    sum = sum + x
    i = i + 1
  }

  val letters = List("a","b","c","d","a","a","a","a")

  def counter(input: List[String], acc: Int = 100): List[Int] = {
    input match {
      case _ if input.length == 1 && input.head == "a" => List(acc + 1)
      case _ if input.length == 1 => List(acc)
      case _ if input.head == "a" => counter(input.tail, acc + 1)
      case _ if input.nonEmpty => counter(input.tail, acc)
      case _ => List(acc)
    }
  }

  println(counter((letters)))



  val listOne = List("A","B")
  val listTwo = List("N","R")
  val listThree = List("D","O")
  val listFour = List("Y","W")

 val day5input = "    [H]         [H]         [V]    \n    [V]         [V] [J]     [F] [F]\n    [S] [L]     [M] [B]     [L] [J]\n    [C] [N] [B] [W] [D]     [D] [M]\n[G] [L] [M] [S] [S] [C]     [T] [V]\n 1   2   3   4   5   6   7   8   9 "
   .split("\n").toList.map(_.replace("         ","x").replace("    ","x"))

  println(day5input)


  val slice: List[List[String]] = List(List("move", "test", "from", "2", "to", "1"), List("move", "1", "from", "2", "to", "1"), List("move", "1", "from", "2", "to", "1"))

  println(slice(0))

  val listToFold = List("10","20","30","40")

  val listToMap = List(1,2,3,4)
  val secondList = Map(1 -> "A", 2 -> "N", 3 -> "D", 4 -> "Y")

//  val xxxxxx = listToMap.foldLeft(secondList) ((acc, curr) => acc.)
//
//  println(xxxxxx)


  val listInt = List(1,2,3,4,5,6)
  val listString = List("a", "b", "c")

val xyz = {

  for {
    int <- listInt
    string <- listString
  } yield (int, string)
}

  println(xyz)

  val groupBy = "aabdegabgaedbc"

  println(groupBy.groupBy(x => x))


}
