package cuttingedge.progAnalysis

import ast._

object flowGraph {

  /**number the statements*/
  def numStmt(s: Stmt, i: Int): Stmt = {
    s match {
      case Stmt.Stmts(xs) =>
        Stmt.Stmts(xs.zipWithIndex map { x =>
          numStmt(x._1, x._2 + i + 1)
        })
      case x @ Stmt.Start(ln)      => x.copy(ln = i)
      case x @ Stmt.End(ln)        => x.copy(ln = i)
      case x @ Stmt.`:=`(l, ln, a) => x.copy(ln = i)
      case Stmt.If_(b, ln, s1, s2) =>
        Stmt.If_(b, i, numStmt(s1, i + 1), numStmt(s2, i + 2))
      case Stmt.While_(b, ln, s) => Stmt.While_(b, i, numStmt(s, i + 1))
      //      case Expr.T(ln)              => Expr.T(i)
      case x =>
        throw new Exception("numStmt error! " + x)
      //      case _                       => s
      //      case Stmt.R(l, a, b)             =>
    }
  }

  def initSet(s: Stmt): Stmt = {
    s match {
      case Stmt.Stmts(xs) => xs.head
      case x              => x

    }
  }

  // todo
  def stmtLab(s: Stmt) = s match {
    case Stmt.`:=`(l, ln, a)     => ln
    case Stmt.If_(b, ln, s1, s2) => ln
    case Stmt.Start(ln)          => ln
    case Stmt.End(ln)            => ln
    case Stmt.While_(b, ln, s)   => ln
    case x: Expr.loc             => x.ln
    case x =>
      throw new Exception("stmtLab error! " + x)
      println("stmtLab error! " + x)
      -1
    //    case Stmt.Stmts(xs) =>
    //    case Stmt.R(l, a, b) =>

    //    case Stmt.While_(b, ln, s) =>
  }

  def labels(s: Stmt): List[Int] = { blocks(s) map stmtLab }

  def finalSet(s: Stmt): List[Stmt] = s match {
    case Stmt.Stmts(xs)          => xs.reverse.headOption.toList
    case Stmt.If_(b, ln, s1, s2) => List(s1, s2)
    case x                       => List(x)
    //    case Stmt.`:=`(l, ln, a) =>
    //    case Stmt.R(l, a, b) =>
  }

  def blocks(st: Stmt): List[Stmt] = st match {
    case expr: Expr              => ???
    case Stmt.Stmts(xs)          => xs flatMap (x => blocks(x))
    case x @ Stmt.`:=`(l, ln, a) => List(x)
    case x @ Stmt.Start(ln)      => List(x)
    case x @ Stmt.End(ln)        => List(x)
    case x @ Stmt.Tup(l, a, b)   => ???
    case Stmt.If_(b, ln, s1, s2) => (b +: blocks(s1)) ++ blocks(s2)
    case Stmt.While_(b, ln, s)   => Expr.loc(b, ln) +: blocks(s)
  }

  /**flow graph*/
  def flowG(s: Stmt): List[(Int, Int)] = {
//    println("flowG")
//    pp(s)
    s match {
      case Stmt.If_(b, bln, s1, s2) =>
        flowG(s1) ++ flowG(s2) ++ List(
          (bln, stmtLab(initSet(s1))),
          (bln, stmtLab(initSet(s2)))
        )
      case Stmt.While_(b, ln, s) =>
        List((ln, stmtLab(initSet(s)))) ++ flowG(s) ++ finalSet(s).map(
          sf => (stmtLab(sf), ln)
        )
      case Stmt.Stmts(xs) =>
        xs match {
          case x :: Nil => List.empty
          case s1 :: ss =>
            // must flatten,remove this extra layer or flatten
            val stmtsS2 = Stmt.Stmts(ss)
            flowG(s1) ++ flowG(stmtsS2) ++ finalSet(s1).map { s_final =>
              //            pp(s_final) //stmts list
              val s_init = initSet(stmtsS2)
//              pp(s_init)
              (stmtLab(s_final), stmtLab(s_init))
            }
          case _ => List.empty
        }
      case _ => List()
    }
  }
}
