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

    // fill array of desired length with zeroes
    def fillWithZeroes(num:Int) : List[Int] = List.fill(num)(0)

    // remove all of the elements of the list at odd indexes
    def removeOdds(arr:List[Int]):List[Int] = arr match {
      case Nil => Nil
      case h :: Nil => Nil
      case _ :: tail => tail.head :: removeOdds(tail.tail)
    }

//    You are incharge of data transfer between two data-centres. Each set of data is represented by a pair of strings.
//    Over a period of time you have observed a trend: most of the times both strings share some prefix.
//    You want to utilize this observation to design a data compression algorithm which will be used to reduce amount of
//    data to be transferred.
//
//    You are given two strings, xx and yy, representing the data, you need to find the longest common prefix (pp) of
//    the two strings. Then you will send substring pp, x′x′ and y′y′, where x′x′ and y′y′ are the substring left after
//    stripping pp from them.
//
//    For example, if x=‘‘abcdefpr"x=‘‘abcdefpr" and y=‘‘abcpqr"y=‘‘abcpqr",
//    then p=‘‘abc",x′=‘‘defpr",y′=‘‘pqr"p=‘‘abc",x′=‘‘defpr",y′=‘‘pqr".
    // *** Input required using def main(...) ***
    def mn(args: Array[String]) {
      // Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution

      val A=Console.readLine()
      val B=Console.readLine()
      var count: Int = 0
      val shared: String =
        A.takeWhile(a => {
          if (B.size == count) false // so we don't ever look for a character past the length of B
          else {
            count += 1
            a == B(count - 1)
          }
        })
      val sharedSize = shared.size
      val aClipped = A.drop(sharedSize)
      val bClipped = B.drop(sharedSize)
      println(shared.size + " " + shared)
      println(aClipped.size + " " + aClipped)
      println(bClipped.size + " " + bClipped)
    }


    // Display every possible rotation of a string - for example 'hey' could be represented as 'hey', 'eyh', and 'yhe'- but not 'hye'
    // *** Input required using def main(...) ***
    def mn2(args: Array[String]) {
      val numLines = Console.readLine.toInt
      for (j <- 1 to numLines) {
        val ln = Console.readLine
        println(rotate(ln, ln.size))
      }
      // first letter to end
      def rotate(str: String, n: Int): String = {
        val rotated: String = str.tail + str.head
        if (n == 1)
          rotated
        else
          rotated + " " + rotate(rotated, n - 1)
      }
    }

    // filter list based on some delimitator
    def delimit(delim:Int,arr:List[Int]):List[Int] = arr.filter(_ < delim)

    // Compute fibonacci number at index: x -- where 1 is starting index
    def fibonacci(x:Int):Int = {
      def recurse(prev: Int, newest: Int, step: Int): Int = {
        if (step == 1)
          prev + newest
        else
          recurse(newest, prev + newest, step - 1)
      }
      if (x == 1)
        0
      else if (x == 2)
        1
      else
        recurse(0, 1, x - 2)
    }

    // return absolute values of list of integers
    def abs(arr:List[Int]):List[Int] = arr.map(ele => Math.abs(ele))

    // remove all characters from string that occur more than once (first character stays)
    def removeExtras(str: String): String = str match {
      case end if (str.length < 2) => end
      case a => a.head + removeExtras(a.tail.filter(_ != a.head))
    }

    // compute GCD
    def gcd(x: Int, y: Int): Int =
      if (x == y)
        x
      else if (x > y)
        gcd(x-y, y)
      else
        gcd(x, y-x)


//    The series expansion of ex is given by:
//
//    1 + x + x2/2! + x3/3! + x4/4! + .......
    def f(x: Float):Float= {
      def factorial(n:Float):Float = if(n==0) 1 else n * factorial(n-1)
      def pow(x: Float, y: Float): Float =
        if (y.toInt == 0)
          1
        else if (y.toInt == 1)
          x
        else
          x * pow(x, y-1)
      def recurse(base: Float, step: Int): Float = {
        if (step > 0)
          (pow(base, step) / factorial(step)) + recurse(base, step - 1)
        else
          1
      }
      recurse(x, 9)
    }

    // given a list - copy each element in the list 'num' times --- supposed to be done with recursion
    def listReplication(num:Int,arr:List[Int]):List[Int] = {
      def makeList(n: Int, i: Int): List[Int] = {
        if (n > 0)
          i :: makeList(n - 1, i)
        else
          Nil
      }
      arr match {
        case Nil => Nil
        case h :: tail => makeList(num, h) ::: listReplication(num, tail)
        case _ => Nil
      }
    }

  }
}