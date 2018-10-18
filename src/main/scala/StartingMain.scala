object StartingMain {
  def main(args: Array[String]): Unit = {
    println(findUnique("a simple string"))
    transformWord("computer", "laptop")

  }

  def findUnique(inputString: String): String = {
    val repeatingLetters = inputString.toLowerCase diff inputString.toLowerCase.distinct
    inputString filter (v => !(repeatingLetters contains v))
  }

  def transformWord(firstString: String, secondString: String):Unit = {
    val uniqueInFirst = firstString diff secondString
    val uniqueInSecond = secondString diff firstString
    val sharedLetters = firstString diff uniqueInFirst
    println(s"To transform $firstString into $secondString you need to do the following steps: \nStart with $firstString" +
      s"\nRemove the letters $uniqueInFirst so that you are left with $sharedLetters \nNow added $uniqueInSecond and rearrange " +
      s"you will be left with $secondString \nTo do this sequence you will remove ${uniqueInFirst.length} letters and add ${uniqueInSecond.length}")
  }

}