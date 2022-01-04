package cuttingedge.progAnalysis

import cuttingedge.progAnalysis.ast.{Expr, Stmt}

object progGraph {
  type progGraphTup = (Int, Stmt, Int)

  /** program graph*/
  def genProgGraph(tup: progGraphTup, usedList: List[Int]): List[progGraphTup] = {
    val (p, stmt, q) = tup
//    println(s"progGraph tup : ${tup}")
    assert(p != q)

    stmt match {
//      case x @ Stmt.`:=`(l, ln, a) => List((p, x, q))
      case Stmt.If_(b, ln, s1, s2) =>
        val i  = usedList.max
        val q1 = i + 1
        val q2 = i + 2
        val ul = usedList ++ List(q1, q2)
        List((p, b, q1), (p, Expr.Not(b), q2)) ++ genProgGraph((q1, s1, q), ul) ++ genProgGraph((q2, s2, q), ul)

      case Stmt.While_(b, ln, s) =>
        val qn = usedList.max + 1
        val ul = usedList ++ List(qn)
        assert(qn != p && qn != q)
        List((p, b, qn), (p, Expr.Not(b), q)) ++ genProgGraph((qn, s, p), ul)

      case Stmt.Stmts(xs) =>
        xs match {
          case x :: Nil => genProgGraph((p, x, q), usedList)
          case s1 :: ss =>
            val qn = usedList.max + 1
            val ul = usedList ++ List(qn)
            genProgGraph((p, s1, qn), ul) ++ genProgGraph((qn, Stmt.Stmts(ss), q), ul)
          case Nil => List.empty // List(tup)
        }
      case _ => List(tup)
    }
  }

  def test = {
    val s = microC_examples.stmt_report_interval
    val r = genProgGraph((0, s, 1), List(0, 1))
    pp(r, "progGraph")
  }
}
