package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println(countChange(300,List(500,5,50,100,20,200,10)))
  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if ( r== 0 || c == 0 || r == c) 1
    else {
      pascal(c-1, r-1) + pascal(c, r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def check(restChars:List[Char], pList:List[Char]): Boolean ={

      if (restChars.isEmpty && pList.nonEmpty) return false
      else if(restChars.isEmpty && pList.isEmpty) return true

      if(restChars.head == '(') {
        check(restChars.tail, pList ::: List('('))
      }
      else if(restChars.head == ')'){
        if (pList.isEmpty) return false
        else check(restChars.tail, pList.tail)
      }
      else check(restChars.tail, pList)
    }

    check(chars, Nil)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    if(money == 0) return 1
    if(money < 0 || coins.isEmpty) return 0

    var newCoins:List[Int] = coins
    var result = 0

    while (newCoins.nonEmpty){
      result += countChange(money - newCoins.head, newCoins)
      newCoins = newCoins.tail
    }

    result
  }
