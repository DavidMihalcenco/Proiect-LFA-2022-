import Nfa.fromPrenex

class Dfa[A] (/* TODO : define the constructor params */){

  var stateNr = 0
  var stat : Set[Map[Set[GRF[A]],Set[Map[String,Set[GRF[A]]]]]] = Set.empty
  var states : Set[Map[A,Set[GRF[A]]]] = Set.empty

  def map[B](f: A => B) : Dfa[B] = ??? // TODO implement map

  def next(state:A, c: Char): A = ??? // TODO implement next

  def getTransitions(f_grup: Set[GRF[A]] => Boolean, f_stare: GRF[A] => Boolean) :
  (Set[GRF[A]],Set[Map[String, Set[GRF[A]]]]) = {
    for (tuplu <- stat) {
      for ((grupStari, tranzitiiGrup) <- tuplu) {
        if(f_grup(grupStari)) {
          return (grupStari, tranzitiiGrup)
        }
        for (stare <- grupStari) {
          if (f_stare(stare)) {
            return (grupStari, tranzitiiGrup)
          }
        }
      }
    }
    Tuple2(Set.empty, Set.empty)
  }

  def accepts(str: String): Boolean = {
    var grupCurent = Set.empty[GRF[A]]
    var tranzitii = Set.empty[Map[String,Set[GRF[A]]]]
    var ret = getTransitions(_ => false ,_.first)
    grupCurent = ret._1
    tranzitii = ret._2
    if(grupCurent.isEmpty) return false
    var stringParcurs = str
    while(stringParcurs.nonEmpty) {
      var charToString = stringParcurs.head.toString
      if(stringParcurs.equals("eps")) charToString = stringParcurs
      stringParcurs = stringParcurs.drop(charToString.length)
      var found = false
      for(tuplu <- tranzitii) {
        for((nextStr, nextGroup) <- tuplu) {
          if(charToString.toString.equals(nextStr)) {
            grupCurent = nextGroup
            ret = getTransitions(_.equals(nextGroup), _ => false)
            tranzitii = ret._2
            found = true
          }
        }

      }
      if(!found) return false
    }
    if(stringParcurs.nonEmpty) return false
    for(stare <- grupCurent) {
      if(stare.last) {
        return true
      }
    }
    false
  }

  def getStates : Set[A] = ??? // TODO implement getStates

  def isFinal(state: A): Boolean = ???  // TODO implement isFinal
}

object Dfa {
  def fromPrenex(str: String): Dfa[Int] = {
    val dfa : Dfa[Int] = new Dfa[Int]
    val nfa = Nfa.fromPrenex(str)
    println()
    val startN = reachGrafEps(nfa.finalGrapf,nfa.finalGrapf)
    reachChar(startN,nfa,dfa)
    dfa
  } // TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa

  def reachGrafEps(set1:Set[GRF[Int]],setf:Set[GRF[Int]]): Set[GRF[Int]] = {
    var setf1 :Set[GRF[Int]] = set1
    var setf2 :Set[GRF[Int]] = setf
    if(setf1.isEmpty){
      return setf2
    }
   for(graph <- setf1){
     for((graph,name) <- graph.list){
       if(name == "eps"){
         if(!setf2.contains(graph)) {
          setf1 += graph
         }
         setf2 += graph
       }
     }
   }
    reachGrafEps(setf1.tail,setf2)
  }

  def reachChar(states : Set[GRF[Int]],nfa : Nfa[Int], dfa : Dfa[Int]): Unit ={
    for (dfas <- dfa.stat) {
      if (dfas.head._1 == states) {
        return
      }
    }
    var set : Set[Map[String,Set[GRF[Int]]]] = Set.empty
    var tryS : Map[Set[GRF[Int]],Set[Map[String,Set[GRF[Int]]]]] = Map.empty
    for (ch <- nfa.alphabet) {
      var mapCH : Map[String,Set[GRF[Int]]] = Map.empty
      var state : Set[GRF[Int]] = Set.empty
      for(sGraf <- states){
        for ((graf, name) <- sGraf.list) {
          if (name == ch) {
            state += graf
          }
        }
      }

      mapCH = Map(ch -> reachGrafEps(state,state))
      set += mapCH
    }

    dfa.states += Map(dfa.stateNr -> states)
    dfa.stateNr += 1
    tryS = Map(states -> set)
    dfa.stat += tryS
    for (set1 <- set) {
     var str = set1.head._2
      for(dfas <- dfa.stat){
        if(dfas.head._1 != str){
          reachChar(str, nfa, dfa)
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    var dfa = fromPrenex("UNION UNION UNION UNION UNION UNION 0 1 2 3 4 5 6")
    println(fromPrenex("UNION UNION UNION UNION UNION UNION 0 1 2 3 4 5 6").accepts("7"))
    println()
  }

}
