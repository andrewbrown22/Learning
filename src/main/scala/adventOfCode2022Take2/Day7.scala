package adventOfCode2022Take2

object Day7 extends App {

  // cd X - moves in one level
  // cd .. - moves out one level
  // cd / - switches the current directory to the outermost directory
  // abc 123 - file 'abc' is 123 in current directory
  // dir xyz - current directory contains a directory called xyz

  val day7TestData = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"
    .split("\n").toList.map(_.replace("$ ", "").trim).filterNot(_ == "ls")

  println(day7TestData)

  case class Directory(name: String)

  case class File(name: String, size: Int)

  case class ChangeDir(name: String)

//  val convertData = day7TestData match {
//      case s"cd $dirName" => ChangeDir(dirName)
//      case s"dir $dirName" => Directory(dirName)
//      case s"$size $name" => File(name, size.toInt)
//
//  }

  // val foldLeft = day7TestData.foldLeft(List(String)) {}


}
