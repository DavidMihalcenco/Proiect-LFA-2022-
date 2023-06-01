import Ast.{assambledAst, createList}
class Nfa[A]() {
  var stateNR = 0
  var allStates: Set[GRF[A]] = Set()
  var finalGrapf : Set[GRF[A]] = Set.empty
  var alphabet : Set[String] = Set.empty

  def map[B](f: A => B) : Nfa[B] = ??? // TODO implement map

  def next(state:A, c: Char): Set[A] = ??? // TODO implement next

  def accepts(str: String): Boolean = ??? // TODO implement accepts

  def getStates : Set[A] = ??? // TODO implement getStates

  def isFinal(state: A): Boolean = ???  // TODO implement isFinal
}

object Nfa {
  var nfa: Nfa[Int] = new Nfa();

  def fromPrenex(str: String): Nfa[Int] = {
    nfa = new Nfa();
    var ast : Ast = Ast.assambledAst(Ast.createList(str))
    var eval = astEval(ast)
    nfa.finalGrapf += eval
    nfa
  }

  def astEval(ast: Ast):GRF[Int] = {

    if (ast.elem == "CONCAT") {

      workWithConcat(ast)
    } else if (ast.elem == "STAR") {

      workWithStar(ast)
    } else if (ast.elem == "UNION") {

      workWithUnion(ast)
    } else if (ast.elem == "PLUS") {

      var ast1 : Ast = new Ast("STAR",ast.left,null)
      var ast2 : Ast = new Ast("CONCAT",ast.left,ast1)
      astEval(ast2)
    } else if (ast.elem == "MAYBE") {
      var ast1: Ast = new Ast("eps", null, null)
      var ast2: Ast = new Ast("CONCAT", ast.left, ast1)
      astEval(ast2)
    } else if (ast.elem == "eps") {

      val grf1: GRF[Int] = new GRF[Int](nfa.stateNR, true, false,
        List.empty[(GRF[Int], String)], List.empty[GRF[Int]])
      val grf2: GRF[Int] = new GRF[Int]((nfa.stateNR + 1), false, true,
        List.empty[(GRF[Int], String)], List.empty[GRF[Int]])
      grf2.finalState :+= grf2

      nfa.stateNR += 2
      nfa.allStates += grf1
      nfa.allStates += grf2
      GRF.addNode(grf1, grf2, "eps")
      grf1

    } else if (ast.elem == "void") {
      val grf1: GRF[Int] = new GRF[Int](nfa.stateNR, true, false,
        List.empty[(GRF[Int], String)], List.empty[GRF[Int]])
      val grf2: GRF[Int] = new GRF[Int]((nfa.stateNR + 1), false, true,
        List.empty[(GRF[Int], String)], List.empty[GRF[Int]])

      grf2.finalState :+= grf2
      grf1
    } else {

      val grf1: GRF[Int] = new GRF[Int](nfa.stateNR, true, false,
        List.empty[(GRF[Int],String)],List.empty[GRF[Int]])
      val grf2: GRF[Int] = new GRF[Int]((nfa.stateNR + 1), false, true,
        List.empty[(GRF[Int],String)],List.empty[GRF[Int]])
      grf2.finalState :+= grf2

      GRF.addNode(grf1,grf2,ast.elem)

      nfa.stateNR += 2
      nfa.allStates += grf1
      nfa.allStates += grf2
      nfa.alphabet += ast.elem

      grf1
    }
  }

  def workWithConcat(ast: Ast):GRF[Int] = {
    val grf1: GRF[Int] = astEval(ast.left)
    val grf2: GRF[Int] = astEval(ast.right)

    grf2.first = false

    GRF.addFinalNode(grf1,grf2,"eps")
    grf1.finalState.head.last = false

    GRF.replaceFinalstate(grf1,grf2)

    grf1
  }

  def workWithUnion(ast: Ast):GRF[Int] ={

    val grf1: GRF[Int] = new GRF[Int](nfa.stateNR, true, false,
      List.empty[(GRF[Int],String)],List.empty[GRF[Int]])
    nfa.stateNR += 1
    nfa.allStates += grf1

    val grf2: GRF[Int]= astEval(ast.left)
    val grf3: GRF[Int] = astEval(ast.right)
    grf2.first = false
    grf3.first = false

    val grf4: GRF[Int] = new GRF(nfa.stateNR, false, true,
      List.empty[(GRF[Int],String)],List.empty[GRF[Int]])
    grf4.finalState :+= grf4
    nfa.stateNR += 1
    nfa.allStates += grf4

    GRF.addFinalNode(grf2,grf4,"eps")
    GRF.addFinalNode(grf3,grf4,"eps")

    grf2.finalState.head.last = false
    grf3.finalState.head.last = false

    GRF.replaceFinalstate(grf2,grf4)
    GRF.replaceFinalstate(grf3,grf4)

    GRF.addNode(grf1,grf2,"eps")
    GRF.addNode(grf1,grf3,"eps")

    GRF.replaceFinalstate(grf1,grf4)

    grf1
  }

  def workWithStar(ast: Ast):GRF[Int] ={
    val grf1: GRF[Int] = new GRF[Int](nfa.stateNR, true, false,
      List.empty[(GRF[Int],String)],List.empty[GRF[Int]])
    nfa.stateNR += 1
    nfa.allStates += grf1

    val grf2: GRF[Int] = astEval(ast.left)
    grf2.first = false

    GRF.addFinalNode(grf2,grf2,"eps")
    GRF.addNode(grf1,grf2,"eps")

    val grf3: GRF[Int] = new GRF[Int](nfa.stateNR, false, true,
      List.empty[(GRF[Int],String)],List.empty[GRF[Int]])
    grf3.finalState :+= grf3

    nfa.stateNR += 1
    nfa.allStates += grf3

    GRF.addFinalNode(grf2,grf3,"eps")
    grf2.finalState.head.last = false
    GRF.replaceFinalstate(grf2,grf3)

    GRF.addNode(grf1,grf3,"eps")
    GRF.replaceFinalstate(grf1,grf3)

    grf1
  }

  def main(args: Array[String]): Unit = {
    nfa = fromPrenex("UNION UNION UNION UNION UNION UNION 0 1 2 3 4 5 6")
    println()
  }
}