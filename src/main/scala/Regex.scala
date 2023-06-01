import scala.collection.immutable.NumericRange
import scala.collection.mutable

class Regex {

}

object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  def process(c1 : Char, c2: Char) : String = {
    var charList = "("
    var x = (c1 to c2)
    x.foreach(y => charList += y.toString + "|")
    charList = charList.dropRight(1)
    charList += ")"
    charList
  }

  def preprocess(s:String): String = {
    var i = 0;
    var charFinal = new String()
    while(i < s.length){
      if(s.charAt(i) == '['){
        var charF = s.charAt(i+1)
        var charS = s.charAt(i+3)
        charFinal += process(charF,charS)
        i += 5
      }
    }
    charFinal
  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    var fString = new String()
    var wString = new String()
   // var pString = new String()
    //pString = preprocess(str)
    wString = infixToPrefix(str)
    for (i <- 0 until wString.length) {
      if(wString.charAt(i) == '|'){
        fString += "UNION "
      }
      if (wString.charAt(i) == '&') {
        fString += "CONCAT "
      }
      if (wString.charAt(i) == '*') {
        fString += "STAR "
      }
      if(!isOperator(wString.charAt(i))){
        fString += wString.charAt(i) + " "
      }
    }
    fString.dropRight(1)
  }

  def isOperator(elem: Char): Boolean = {
    !((elem >= 'a' && elem <= 'z')) &&
    !((elem >= '0' && elem <= '9')) &&
    !(elem >= 'A' && elem <= 'Z')
  }

  def getPriority(elem : Char): Int = {
    if (elem == '|') return 1
    else if (elem == '&') return 2 // concat
    else if (elem == '*') return 3
    0
  }

  def infixToPrefix(toWork: String): String = {

    val operators = mutable.Stack[Char]()
    val operands = mutable.Stack[String]()

    for (i <- 0 until toWork.length) {
      var currentOperation = ' '
      if(i > 0) {
        if(toWork.charAt(i) == '*'){
          currentOperation = '*'
        }
        if (toWork.charAt(i) == '|') {
          currentOperation = '|'
        }
        if (toWork.charAt(i-1) == ')' && !isOperator(toWork.charAt(i))) {
          currentOperation = '&'
        }
        if (toWork.charAt(i-1) == ')' && toWork.charAt(i) == '(') {
          currentOperation = '&'
        }
        if (!isOperator(toWork.charAt(i-1)) && toWork.charAt(i) == '(') {
          currentOperation = '&'
        }
        if (!isOperator(toWork.charAt(i-1)) && !isOperator(toWork.charAt(i))) {
          currentOperation = '&'
        }
        if (toWork.charAt(i-1) == '*' && !isOperator(toWork.charAt(i))) {
          currentOperation = '&'
        }
        if (toWork.charAt(i - 1) == '*' && toWork.charAt(i) == '(') {
          currentOperation = '&'
        }
      }

      if (currentOperation == '&' || currentOperation == '*' || currentOperation == '|') {
        while (operators.nonEmpty && getPriority(currentOperation) <= getPriority(operators.top)) {
          if (operators.top == '*') {
            val op = operands.top
            operands.pop
            val op1 = operators.top
            operators.pop
            val tmp = op1 + op
            operands.push(tmp)
          } else {
            val op1 = operands.top
            operands.pop
            val op2 = operands.top
            operands.pop
            val op = operators.top
            operators.pop
            val tmp = op + op2 + op1
            operands.push(tmp)
          }
        }
        operators.push(currentOperation)

      }

      if (toWork.charAt(i) == '('){
        operators.push(toWork.charAt(i))
      }else if (toWork.charAt(i) == ')') {
          while (operators.nonEmpty &&
            operators.top != '(') {
            if(operators.top == '*'){
              val op = operands.top
              operands.pop
              val op1 = operators.top
              operators.pop
              val tmp = op1 + op
              operands.push(tmp)
            }else {
              val op1 = operands.top
              operands.pop
              val op2 = operands.top
              operands.pop
              val op = operators.top
              operators.pop
              val tmp = op + op2 + op1
              operands.push(tmp)
            }
          }
          operators.pop
        } else if (!isOperator(toWork.charAt(i))){
          operands.push(toWork.charAt(i) + "")
          }
    }
    while (operators.nonEmpty) {
      if (operators.top == '*') {
        val op = operands.top
        operands.pop
        val op1 = operators.top
        operators.pop
        val tmp1 = op1 + op
        operands.push(tmp1)
      } else {
        val op1 = operands.top
        operands.pop
        val op2 = operands.top
        operands.pop
        val op = operators.top
        operators.pop
        val tmp = op + op2 + op1
        operands.push(tmp)
      }
    }
    operands.top
  }

  def main(args: Array[String]): Unit = {
    val s = preprocess("[0-9]")
    println(s)
    println(infixToPrefix(s))
    println(toPrenex(s))
  }
}
