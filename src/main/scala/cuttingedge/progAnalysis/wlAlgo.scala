package cuttingedge.progAnalysis

import cuttingedge.progAnalysis.ast.Stmt
import cuttingedge.progAnalysis.progGraph._
import cuttingedge.progAnalysis.flowGraph._
import scala.collection.mutable
import scala.reflect.ClassTag

/**generic Worklist algorithm*/
object wlAlgo {
  trait Worklist[t] {
    def extract: t
    def insert(e: t): Unit
    def insertAll(e: Seq[t]): Unit
    def isEmpty: Boolean
//    def empty: Worklist[t]
  }

  class WlStack[t: ClassTag](as: mutable.ArrayStack[t]) extends Worklist[t] {
    override def extract: t = as.pop()

    override def insert(e: t): Unit = as += e

    override def isEmpty: Boolean = as.isEmpty

//    override def empty: Worklist[t] = new WlStack[t](mutable.ArrayStack[t]())

    override def insertAll(e: Seq[t]): Unit = as ++= e
  }

  /**
    * generic Worklist algorithm for program graph
    * @param stmtGlob
    * @param transferF transfer function
    * @param subOrderOp ordering for lattice
    * @param lubOp least upper bound operator for lattice
    * @param initD initial lattice element for first program point
    * @param bottomD bottom(least) element for lattice
    * @param isReverse forward or reverse analysis
    * @tparam domain the lattice type
    * @return Map[Int, domain] giving the result at all program points
    */
  def wlAlgoProgGraph[domain](
      wlMut: Worklist[Int],
      stmtGlob: Stmt,
      transferF: (Stmt, domain) => domain,
      subOrderOp: (domain, domain) => Boolean,
      lubOp: (domain, domain) => domain,
      initD: domain,
      bottomD: domain,
      isReverse: Boolean = false
  ): Map[Int, domain] = {
    pp(stmtGlob, "input program")

    val progG_fw = progGraph.genProgGraph((0, stmtGlob, -1), List(0, -1)) //init node is 0,final node is -1

    val progG =
      if (isReverse) progG_fw map (x => (x._3, x._2, x._1)) else progG_fw

    pp(progG, "program Graph")

//    get program points from edges
    val Q = progG.flatMap(x => List(x._1, x._3)).distinct

//    initialise work list
//    val wlMut: mutable.ArrayStack[Int] = mutable.ArrayStack(Q: _*) //flowG: _*
    wlMut.insertAll(Q)

    val resMapMut
        : mutable.Map[Int, domain] = mutable.Map() // 2 -> Set(Var("x"))

//    initialize results at program points
    Q foreach { q =>
      resMapMut(q) = if (q == 0) initD else bottomD
    }

//    pp(resMapMut.toMap, "init resMap : ")
    //second loop
    var steps = 0
    while (!wlMut.isEmpty) {
      steps += 1
//      pp(wlMut.toList, "work list : ")

      val q_wl = wlMut.extract //        extract

      val progGraphTups = progG.filter(_._1 == q_wl) // prog point tuples after q_wl

      progGraphTups foreach {
        case tup @ (pre, st, post) =>
          val preMap = resMapMut(pre)
          val postMap = resMapMut(post) //AA(q dot)
          val preMapTransfered = transferF(st, preMap)

//          update if preMapAnalysised >=  postMap (not <=)
          val doUpdate = !subOrderOp(preMapTransfered, postMap)

//          println(s"doUpdate if presetF ${pre} notSubOrder postset ${post}:: at ${post}", doUpdate)
//          pp(preMapTransfered, s"preSetF f(${pre}):")
//          pp(postMap, s"postSet ${post}:")

          if (doUpdate) {

            val lubR = lubOp(postMap, preMapTransfered)
//            pp(lubR, "lub : ")
            resMapMut(post) = lubR
//            wlMut += post
            wlMut.insert(post)
          } else {
//            println(s"no Update for ${edge} ")
          }
      }
    }
    println(s"iter step : ${steps}")
    resMapMut.toMap
  }

  /** generic Worklist algorithm for flow graph
    * domain can be:
    * semantics : var->z,
    * det of signs :  var->signs,
    * live vars : var
    * reaching defs : var*label
    * */
  def wlAlgoFlowGraph[domain](
      sGlob: Stmt,
      analysisF: (Stmt, domain) => domain,
      subOrderOp: (domain, domain) => Boolean,
      lubOp: (domain, domain) => domain,
      initD: domain,
      bottomD: domain,
      isReverse: Boolean = false
  ): Map[Int, domain] = {
    val flowg_fw = flowGraph.flowG(sGlob)

    val flowG = if (isReverse) flowg_fw map (x => (x._2, x._1)) else flowg_fw

    println(s"flowg is rev? ${isReverse} : " + flowG)

    val finalSetLab = finalSet(sGlob).map(stmtLab) // final s*
    val blocks_lab = blocks(sGlob).map(x => (stmtLab(x), x))
    val lab2stmt = { l: Int =>
      blocks_lab.find(_._1 == l).get._2
    }

    val labs = blocks_lab.map(_._1)

    //      hollow circle,entry init lvExitM as empty,
    val entryMap: mutable.Map[Int, domain] = mutable.Map() // 2 -> Set(Var("x"))
    val setInit = initSet(sGlob)

    //first loop,    init worklist
    val wl: mutable.ArrayStack[(Int, Int)] = mutable.ArrayStack(flowG: _*)

//    init first line  to top elem,other to bottom
    labs foreach { l =>
      val domainInit = if (labels(setInit).head == l) initD else bottomD

      entryMap(l) = domainInit
    }
    println(s"labels(setInit) : ${labels(setInit)} ,labs : $labs")
    pp(entryMap, "worklist init")

    //second loop
    var steps = 0
    while (wl.nonEmpty) {
      steps += 1
      pp(wl.toList, "work list : ")

      val edge @ (l, l_prime) = wl.pop() //        extract

      println("at edge : " + edge)
      //        solid circle,exit
      val exitSet = analysisF(lab2stmt(l), entryMap(l))
      //        println(s"lvExit set ${l} : " + lvExitSet)
      val doUpdate = !subOrderOp(exitSet, entryMap(l_prime))

      if (doUpdate) {
        //          add new analysis set
//        println("doUpdate ? ", doUpdate)
        pp(entryMap(l_prime), s"analysis(l') entryMap before at ${l_prime}: ")
        pp(exitSet, "f(analysis(l)) exitSet:")
        val lub = lubOp(entryMap(l_prime), exitSet)
        entryMap(l_prime) = lub
        pp(lub, "lub of entryMap(l_prime), exitSet")
        pp(entryMap(l_prime), s"entryMap Updated at ${l_prime}: ")

        wl ++= flowG.filter(_._1 == l_prime) // info changed at l' . l,l',l''        insert,3rd loop ,add new edge
      } else {
        println(s"no Update for ${edge} ")
      }

    }
    println(s"iter step : ${steps}")
    entryMap.toMap
  }

}
