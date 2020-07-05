package one.lab.tasks.week.two


object Collections extends App {
  // getLast(List(1 ,2, 3, 4)) -> 4
  // getLast(List())           -> java.util.NoSuchElementException
  def getLast[A](list: List[A]): A =
    list match {
        case Nil                        => throw new NoSuchElementException
        case x :: tail if tail.isEmpty  => x
        case _ :: tail                  => getLast(tail)
    }

  // getLastOption(List(1 ,2, 3, 4)) -> Some(4)
  // getLastOption(List())           -> None
  def getLastOption[A](list: List[A]): Option[A] =
    list match {
        case Nil                        => None
        case x :: tail if tail.isEmpty  => Some(x)
        case _ :: tail                  => getLastOption(tail)
    }
//  println(getLastOption(List(1, 2, 3, 4)))
//  println(getLastOption(List()))

  // getPreLast(List(1 ,2, 3, 4)) -> 3
  // getPreLast(List(1))          -> java.util.NoSuchElementException
  // getPreLast(List())           -> java.util.NoSuchElementException
  def getPreLast[A](list: List[A]): A =
    list match {
        case list if list.size < 2          => throw new NoSuchElementException
        case x :: tail if tail.length == 1  => x
        case _ :: tail                      => getPreLast(tail)
    }
//  println(getPreLast(List(1, 2, 5, -3, 5)))
//  println(getPreLast(List(1, 2, 3)))
//  println(getPreLast(List(99, 100)))


  // getPreLastOption(List(1 ,2, 3, 4)) -> Some(3)
  // getPreLastOption(List(1))          -> None
  // getPreLastOption(List())           -> None
  def getPreLastOption[A](list: List[A]): Option[A] =
    list match {
        case list if list.size < 2        => None
        case x :: tail if tail.size == 1  => Some(x)
        case _ :: tail                    => getPreLastOption(tail)
    }
//  println(getPreLastOption(List(1, 2, 3, 4)))
//  println(getPreLastOption(List(1)))
//  println(getPreLastOption(List()))

  // getNthElement(3, List(1 ,2, 3, 4)) -> 3
  // getNthElement(3, List(1))          -> java.lang.IndexOutOfBoundsException
  def getNthElement[A](n: Int, list: List[A]): A = {
    def getElement[A](i: Int, list: List[A]): A =
      list match {
          case x :: _ if i == n    => x
          case _ :: tail           => getElement(i + 1, tail)
      }
    if (n > list.size || list.isEmpty) throw new NoSuchElementException
    else getElement(1, list)
  }
//  println(getNthElement(3, List(1, 2, 3, 4)))
//  println(getNthElement(2, List(-1, 13, 23, 41)))
//  println(getNthElement(4, List(-1, 13, 23, 41)))
//  println(getNthElement(5, List(-1, 13, 23, 41)))
//  println(getNthElement(3, List(1)))


  // getNthElementOption(3, List(1 ,2, 3, 4)) -> Some(3)
  // getNthElementOption(3, List(1))          -> None
  def getNthElementOption[A](n: Int, list: List[A]): Option[A] = {
    def getElement[A](i: Int, list: List[A]): Option[A] =
      list match {
          case x :: _ if i == n  => Some(x)
          case _ :: tail         => getElement(i + 1, tail)
      }
    if (n > list.size || list.isEmpty) None
    else getElement(1, list)
  }
//  println(getNthElementOption(3, List(1, 2, 3, 4)))
//  println(getNthElementOption(3, List(1)))
//  println(getNthElementOption(2, List(22, 331, 231, 123)))

  // getLength(List(1,2,3)) -> 3
  // getLength(List())      -> 0
  def getLength[A](list: List[A]): Int = {
    def getLength[A](length: Int, list: List[A]): Int =
      list match {
        case Nil        => length
        case _ :: tail  => getLength(length + 1, tail)
      }
    getLength(0, list)
  }
//  println(getLength(List(1, 2, 3)))
//  println(getLength(List()))

  // getReversedList(List(1,2,3)) -> List(3,2,1)
  def getReversedList[A](list: List[A]): List[A] = {
    var reversedList = List[A]()
    def getReversed(list: List[A]): Unit = {
      reversedList = list.head :: reversedList
      if (list.tail.nonEmpty) getReversed(list.tail)
    }
    if (list.nonEmpty) getReversed(list)
    reversedList
  }
//  println(getReversedList(List(2, 5, 1, 23, 15)))
//  println(getReversedList(List(1, 2, 3, 4, 5)))
//  println(getReversedList(List(10)))
//  println(getReversedList(List()))

  // duplicateEveryElement(List(1,2,3)) -> List(1,1,2,2,3,3)
  def duplicateEveryElement[A](list: List[A]): List[A] = {
    var doubledList = List[A]()
    def getDuplicates(list: List[A]): Unit = {
      if (list.tail.nonEmpty) getDuplicates(list.tail)
      doubledList = list.head :: list.head :: doubledList
    }
    if (list.nonEmpty) getDuplicates(list)
    doubledList
  }
//  println(duplicateEveryElement(List(1, 2, 3)))
//  println(duplicateEveryElement(List(2, 13, 23, 100)))
//  println(duplicateEveryElement(List(1)))
//  println(duplicateEveryElement(List()))
}
