class GRF[A](var name: A, var first: Boolean, var last: Boolean, var list: List[(GRF[A],String)],var finalState: List[GRF[A]]) {

}

object GRF{

    def addNode(grf1: GRF[Int],grf2: GRF[Int], name:String ) ={
      grf1.list :+= (grf2 -> name)
      if(grf2.last){
        grf1.finalState :+= grf2
      }
    }

    def addFinalNode(grf1: GRF[Int],grf2: GRF[Int], name:String ): Unit ={
      grf1.finalState.head.list :+= (grf2 -> name)
    }

    def replaceFinalstate(grf1: GRF[Int],grf2: GRF[Int]): Unit ={
      if (grf1.finalState == grf2.finalState){
        return
      }
      grf1.finalState = grf2.finalState
      for((graf, name) <- grf1.list) {
        replaceFinalstate(graf,grf2)
      }

    }


}

