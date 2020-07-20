package one.lab.tasks.week.three

object Collections extends App {

  // duplicateNTimes(3, List(1,2,3)) == List(1,1,1,2,2,2,3,3,3)
  // duplicateNTimes(3, List()) == List()
  def duplicateNTimes[A](n: Int, list: List[A]): List[A] = {
    def getPart[A](i: Int, part: List[A], obj: A): List[A] = {
        if (i < n) getPart(i + 1, obj :: part, obj)
        else obj :: part
    }
    def getBody[A](body: List[A], list: List[A]): List[A] = {
      list match {
        case Nil => body
        case head :: tail => getBody(body ::: getPart(1, Nil, head), tail)
      }
    }
    getBody(Nil, list)
  }
//  println(duplicateNTimes(3, List(1, 2, 3)))
//  println(duplicateNTimes(2, List(-2, 12, 1333, 5)))
//  println(duplicateNTimes(5, List(-1, -5, -2, -4, -3)))
//  println(duplicateNTimes(10, List()))

  // splitAtK(4, List(1,2,3,4,5,6,7,8,9)) == (List(1,2,3,4), List(5,6,7,8,9))
  // splitAtK(0, List(1,2,3)) == (List(), List(1,2,3))

  def splitAtK[A](k: Int, list: List[A]): (List[A], List[A]) = {
    def splitter[A](result: List[A], list: List[A], i: Int): (List[A], List[A]) = {
      list match {
        case Nil          => if (i == k) (result, Nil) else throw new IndexOutOfBoundsException
        case head :: tail => if (i == k) (result, list) else splitter(result :+ head, tail, i + 1)
      }
    }
    splitter(Nil, list, 0);
  }
//  println(splitAtK(6, List(1, 2, 3, 4, 5, 6)))
//  println(splitAtK(4, List(1, 2, 3, 4, 5, 6, 7, 8, 9)))
//  println(splitAtK(0, List(1, 2, 3)))
//  println(splitAtK(3, List(-1, 2, -15, 3, -30, 4)))



  // removeKthElement(5, List(1,2,3,4,5,6)) == (List(1,2,3,4,5), 6)
  // removeKthElement(2, List(1,2,3,4,5,6)) == (List(1,2,4,5,6), 2)
  // removeKthElement(-3, List(1,2,3,4,5,6)) == IndexOutOfBoundException
  // removeKthElement(1000, List(1,2,3,4,5,6)) == IndexOutOfBoundException


  def removeKthElement[A](k: Int, list: List[A]): (List[A], A) = {
    def remover[A](i: Int, result: List[A], list: List[A]): (List[A], A) = {
      list match {
        case Nil          => throw new IndexOutOfBoundsException
        case head :: tail => if (i == k) (result ::: tail, head) else remover(i + 1, result :+ head, tail)
      }
    }
    remover(0, Nil, list)
  }

//  println(removeKthElement(5, List(1, 2, 3, 4, 5, 6)))
//  println(removeKthElement(2, List(1, 2, 3, 4, 5, 6)))
//  println(removeKthElement(-3, List(1, 2, 3, 4, 5, 6)))
//  println(removeKthElement(100, List(1, 2, 3, 4, 5, 6, 7, 8, 9)))
}
