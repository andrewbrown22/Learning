package adventOfCode2022Take2

object Day11 extends App {

  val x = "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1"
  val y = "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3"

  case class Monkey(name: String, startingItem: List[Int], operation: String, test: (String, Int), ifTrue: Int, ifFalse: Int)

  val monkeys: List[Monkey] = x.split("\n\n").toList.map(_.split("\n").toList) map { monkey =>
    val name = monkey.head.replace(":","")
    val startingItems = monkey(1).split(":").last.trim.split(",").map(_.trim.toInt).toList
    val operation = monkey(2).split(":").last.trim
    val test = monkey(3).replace("Test: ", "").replace("by","").trim.split(" ").toList.filterNot(_.isEmpty) match {
      case List(a, b) => (a, b.toInt)
    }
    val ifTrue = monkey(4).last.asDigit
    val ifFalse = monkey(5).last.asDigit
    Monkey(name, startingItems, operation, test, ifTrue, ifFalse)
  }

  def oneRound(monkey: List[Monkey]) = {




  }

  def operationApply(monkey: Monkey) = {
    val firstMonkey: Monkey = monkey
    firstMonkey.operation.split(" ").toList match {
      case List(_, _, _, d, e) if e == "old" && d == "*" => firstMonkey.startingItem.map(x => x * x)
      case List(_, _, _, d, e) if e == "old" && d == "+" => firstMonkey.startingItem.map(x => x + x)
      case List(_, _, _, d, e) if d == "*" => {
          val newE = e.toInt
        firstMonkey.startingItem.map(x => (x * newE) / 3)
      }
      case List(_, _, _, d, e) if d == "+" => {
          val newE = e.toInt
        firstMonkey.startingItem.map(x => x + newE)
      }
    }
  }

  println(operationApply(Monkey("test",List(79,98),"new = old * 19",("", 1), 0, 2)))

  println(test)

}
