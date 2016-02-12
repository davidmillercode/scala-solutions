object ScalaMain {
  def main(args: Array[String]) = {
    //Count the number of elements in an array without using count, size or length operators (or their equivalents).
    // The input and output portions will be handled automatically by the grader. You only need to write a function with
    // the recommended method signature.
    def arrayLength(arr:List[Int]):Int = arr match {
      case Nil => 0
      case h :: Nil => 1
      case h :: t => 1 + arrayLength(t)
    }

    //Return sum of odd elements from an list.
    def sumOfOdds(arr:List[Int]):Int = arr match {
      case Nil => 0
      case h :: Nil if(h % 2 != 0) => h
      case h :: t if(h % 2 != 0) => h + sumOfOdds(t)
      case _ :: t => sumOfOdds(t)
    }

    // reverse a  list without built-ins
    def reverseList(arr:List[Int]):List[Int] = arr match {
      case Nil => Nil
      case h :: t => reverseList(t) ::: List(h)
    }

  }
}