package cuttingedge.progAnalysis

import cuttingedge.progAnalysis.ast.Expr.Var

object ast {
  sealed trait Decl

  object Decl {
    case class int_(l: Var)             extends Decl
    case class intArr(l: Var, len: Int) extends Decl
  }

  case class Prog(ds: List[Decl], stmts: Stmt.Stmts)

  /** We write x for variables, A for array names, R for record names and n for numbers */
  sealed trait Expr   extends Stmt
  sealed trait Expr_l extends Expr
  sealed trait Expr_a extends Expr
  sealed trait Expr_b extends Expr

  object Expr {
    case class Var(x: String) extends Expr with Expr_a with Expr_b with Expr_l
    case object T             extends Expr_b
    case object F             extends Expr_b
    case class Not(x: Expr_b) extends Expr_b

    case class intValue(x: Int)                      extends Expr_a
    case class opA(a: Expr_a, b: Expr_a, op: String) extends Expr_a

    case class opB(a: Expr_b, b: Expr_b, op: String) extends Expr_b
    case class opR(a: Expr_a, b: Expr_a, op: String) extends Expr_b
    case class loc(a: Expr, ln: Int)                 extends Expr_a with Expr_b
  }

  sealed trait Stmt

  object Stmt {

    case class Start(ln: Int = 0) extends Stmt

    case class End(ln: Int = 0) extends Stmt

    case class Stmts(xs: List[Stmt]) extends Stmt

    case class `:=`(nm: Var, ln: Int, a: Expr) extends Stmt

    case class Tup(l: Var, a: Expr_a, b: Expr_a) extends Stmt //tuple

    case class If_(b: Expr_b, ln: Int, s1: Stmt, s2: Stmt) extends Stmt //if

    case class While_(b: Expr_b, ln: Int, s: Stmt) extends Stmt //if

  }

}
