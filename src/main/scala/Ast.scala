import scala.collection.mutable.ListBuffer

class Ast(var elem: String, var left: Ast, var right: Ast) {

}

object Ast{

  def createList(str: String): List[String] = {
    // '@' -> @
    //' ' -> split " " -> (',') -> " "
    val listBuffer = new ListBuffer[String]
    listBuffer.addAll(str.split(' '))
    listBuffer.toList
  }

  def assambledAst(list: List[String]): Ast = {
    var listT: List[String] = list

    def createAst(list1: List[String]): Ast = {
      listT = listT.tail
      val listH = list1.head
      if (listH == "UNION") {
        new Ast(listH, createAst(list1.tail), createAst(listT))
      } else if (listH == "STAR") {
        new Ast(listH, createAst(list1.tail), null)
      } else if (listH == "CONCAT") {
        new Ast(listH, createAst(list1.tail), createAst(listT))
      } else if (listH == "PLUS") {
        new Ast(listH, createAst(list1.tail), null)
      } else if (listH == "MAYBE") {
        new Ast(listH, createAst(list1.tail), null)
      } else {
        new Ast(listH, null, null)
      }
    }
    createAst(list)
  }

  def main(args: Array[String]): Unit = {
  }
}
