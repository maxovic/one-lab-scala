package one.lab.tasks.week.two

import scala.annotation.tailrec


object Collections extends App {
  // getLast(List(1 ,2, 3, 4)) -> 4
  // getLast(List())           -> java.util.NoSuchElementException
  def getLast[A](list: List[A]): A =
    list match {
        case Nil                        => throw new NoSuchElementException
        case x :: Nil                   => x
        case _ :: tail                  => getLast(tail)
    }

  // getLastOption(List(1 ,2, 3, 4)) -> Some(4)
  // getLastOption(List())           -> None
  def getLastOption[A](list: List[A]): Option[A] =
    list match {
        case Nil                        => None
        case x :: Nil                   => Some(x)
        case _ :: tail                  => getLastOption(tail)
    }
//  println(getLastOption(List(1, 2, 3, 4)))
//  println(getLastOption(List()))

  // getPreLast(List(1 ,2, 3, 4)) -> 3
  // getPreLast(List(1))          -> java.util.NoSuchElementException
  // getPreLast(List())           -> java.util.NoSuchElementException
  def getPreLast[A](list: List[A]): A =
    list match {
        case Nil            => throw new NoSuchElementException
        case _ :: Nil       => throw new NoSuchElementException
        case x :: _ :: Nil  => x
        case _ :: tail      => getPreLast(tail)
    }
//  println(getPreLast(List(1, 2, 5, -3, 5)))
//  println(getPreLast(List(1, 2, 3)))
//  println(getPreLast(List(99, 100)))

  // getPreLastOption(List(1 ,2, 3, 4)) -> Some(3)
  // getPreLastOption(List(1))          -> None
  // getPreLastOption(List())           -> None
  def getPreLastOption[A](list: List[A]): Option[A] =
    list match {
        case Nil            => None
        case _ :: Nil       => None
        case x :: _ :: Nil  => Some(x)
        case _ :: tail      => getPreLastOption(tail)
    }
//  println(getPreLastOption(List(1, 2, 3, 4)))
//  println(getPreLastOption(List(1)))
//  println(getPreLastOption(List()))

  // getNthElement(3, List(1 ,2, 3, 4)) -> 3
  // getNthElement(3, List(1))          -> java.lang.IndexOutOfBoundsException
  def getNthElement[A](n: Int, list: List[A]): A =
    list match {
      case x :: tail => if (n == 1) x else getNthElement(n - 1, tail)
    }
//  println(getNthElement(3, List(1, 2, 3, 4)))
//  println(getNthElement(2, List(-1, 13, 23, 41)))
//  println(getNthElement(4, List(-1, 13, 23, 41)))
//  println(getNthElement(5, List(-1, 13, 23, 41)))
//  println(getNthElement(3, List(1)))


  // getNthElementOption(3, List(1 ,2, 3, 4)) -> Some(3)
  // getNthElementOption(3, List(1))          -> None
  def getNthElementOption[A](n: Int, list: List[A]): Option[A] =
    list match {
      case x :: tail => if (n == 1) Some(x) else getNthElementOption(n - 1, tail)
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
    @tailrec
    def getReverse[A](result: List[A], list: List[A]): List[A] =
      list match {
          case Nil        => result
          case x :: tail  => getReverse(x :: result, tail)
      }
    getReverse(Nil, list)
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
