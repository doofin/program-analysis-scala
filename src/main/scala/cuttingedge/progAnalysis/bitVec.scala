package cuttingedge.progAnalysis

import cuttingedge.progAnalysis.ast.Expr.Var
import cuttingedge.progAnalysis.ast._
import cuttingedge.progAnalysis.progGraph._
import cuttingedge.progAnalysis.progGraph.progGraphTup
import monocle._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object bitVec {

  /** variable x or array A might have been last defined (or modified) by the
    action a on an edge (q , a, q' )
    killRd: global info,actually irrelevant of location.Map[var,List(q,q')]
    genRd:local info
    */
  type RDDomain = Map[Var, List[(Int, Int)]]
  def transferRd(tup: progGraphTup)(imm: RDDomain): RDDomain = {
    val (p, s, q) = tup
    s match {
      case Stmt.`:=`(nm, ln, a) => imm.updated(nm, List((p, q)))
      case _                    => imm
    }

  }

  def rdLUB(a: RDDomain, b: RDDomain) = {
    a map { x =>
      (x._1, x._2 ++ b(x._1))
    }
  }

  def rdSuborder(a: RDDomain, b: RDDomain) = {
    true
  }

  /** free var in a stmt,just all var names in a expr
    *  https://www.optics.dev/Monocle/docs/optics/prism
    * */
  def fv(e: Stmt): List[Var] = {
    val pr = Prism[Stmt, Var] {
      case x: Var => Some(x)
      case _      => None
    }(x => x)
    val lits = pr.asFold.getAll(e)
//    pp(lits)
    lits
  }

  /**live var*/
  def genSetLV(s: Stmt): List[Var] = s match {
    case Stmt.`:=`(l, ln, a) => fv(a)
    case expr: Expr          => fv(expr)
    //    case Stmt.Stmts(xs)          =>
    case _ => List()
  }

  /** global info,analysis whole program in bit vec*/
  def killSetLV(s: Stmt): List[Var] = s match {
    case Stmt.`:=`(l, ln, a) => List(l)
//    case Stmt.Stmts(xs)          =>
    case _ => List()
  }

  def transferLV(s: Stmt, sprev: Set[Var]) = {
    val ks = killSetLV(s).toSet
    val gs = genSetLV(s).toSet
    println(s"transferLV stmt : $s , ks : gs: ")
    (sprev -- ks) union gs
  }

  /** reaching def before */
  def rdPrev(ln: Int) = {}

}

/*
  def test(s: Stmt) = {
    val blks = blocks(s)
    blks foreach { b =>
      pp("kill" + killSetLV(b))
      pp("gen" + genSetLV(b))
    }

    lv(s)
  }

  /** reaching def after */
  def testLv() = encloseDebug("test1") {
//    val s = Stmt.stmt3_vars
    val s = microC_examples.stmt4_vars_interval
    //    pprintln(s, width = 5)
    //    pp(numed)
    bitVec.lv(s)

  }

  def lv(sGlob: Stmt) = encloseDebug("lv") {
    val numed     = numStmt(sGlob, 0)
    val flowg     = flowGraph.flowG(numed)
    val flowg_rev = flowg map (x => (x._2, x._1))

    println("flowg_rev : " + flowg_rev)

    val finalSetLab = finalSet(numed).map(stmtLab) // final s*
    val blocks_lab  = blocks(numed).map(x => (stmtLab(x), x))
    val lab2stmt = { l: Int =>
      blocks_lab.find(_._1 == l).get._2
    }

    def lvExit(ln: Int): List[Var] = encloseDebug("lvExit : " + ln) {
      val ll = flowg.find(_._2 == ln).get._1
      println("finalSetLab : " + finalSetLab, ln, ll)
      if (finalSetLab contains ln) {
        List()
      } else {

        lvEntry(ll)
      }
    }

    def lvEntry(ln: Int): List[Var] = encloseDebug("lvEntry : " + ln) {
      //    List().toBuffer -- List()
      val s1: Stmt = blocks_lab.find(_._1 == ln).get._2
      lvExit(ln).filter(x => killSetLV(s1) contains x) ++ genSetLV(s1)
    }

    def wlAlgo(): Unit = {
      val labs                               = blocks_lab.map(_._1)
      val wl: mutable.ArrayStack[(Int, Int)] = mutable.ArrayStack(flowg_rev: _*)

      //      hollow circle,entry init lvExitM as empty,
      val lvEntryMap: mutable.Map[Int, Set[Var]] = mutable.Map() // 2 -> Set(Var("x"))
      val final_set                              = initSet(numed)

      labs foreach { l =>
        lvEntryMap(l) = if (labels(final_set) contains l) {
          fv(numed).toSet
        } else Set()
      }

//second loop
      while (wl.nonEmpty) {
        println("work list : " + wl.toList)

        val edge @ (l, l_prime) = wl.pop() //        extract

//        solid circle,exit
        val lvExitSet = transferLV(lab2stmt(l), lvEntryMap(l))
//        println(s"lvExit set ${l} : " + lvExitSet)
        println("lvEntryMap : " + lvEntryMap.toList)
        if (!(lvExitSet subsetOf lvEntryMap(l_prime))) {
//          add new analysis set
          lvEntryMap(l_prime) = lvEntryMap(l_prime) union lvExitSet

          wl ++= flowg_rev.filter(_._2 == l_prime) //          insert,3rd loop ,add new edge
        }
      }
    }

    wlAlgo()

  }
 */
