package cuttingedge.progAnalysis

import cuttingedge.progAnalysis.ast.Expr
import cuttingedge.progAnalysis.ast.Expr.{Var, intValue}
import cuttingedge.progAnalysis.ast.Stmt.{End, Start, Stmts, While_, `:=`}

object microC_examples {
  //    val stmt1         = If_(Expr.T, 1, `:=`(Lit("a"), 2, Expr.T), `:=`(Lit("b"), 3, Expr.F))
  //    val stmt1 = If_(Expr.T, 0, `:=`(Var("a"), 0, Expr.T), `:=`(Var("b"), 0, Expr.F))
  //    val stmt2_numed =
  //      If_(Expr.T, 1, Stmts(List(`:=`(Var("a"), 2, Expr.T), `:=`(Var("a"), 3, Expr.T))), `:=`(Var("b"), 4, Expr.F))
  //
  //    val stmt3_vars = Stmts(
  //      List(Start(), `:=`(Var("a"), 0, Var("b")), `:=`(Var("b"), 0, Expr.T), `:=`(Var("c"), 0, Var("d")))
  //    )

  val stmt4_vars_interval = Stmts(
    List(
      Start(),
      `:=`(Var("a"), 0, Var("b")),
      `:=`(Var("b"), 0, Expr.intValue(10)),
      `:=`(Var("c"), 0, Expr.intValue(1000)),
      End()
    )
  )

  /*
  y:=1;
  do y<11 -> z:=x*y;
        y:=y+1
  od
   */
  val stmt_report_interval = Stmts(
    List(
      `:=`(Var("y"), 0, Expr.intValue(1)),
      While_(
        Expr.opR(Var("y"), Expr.intValue(11), "<="),
        0,
        Stmts(
          List(
            `:=`(Var("z"), 0, Expr.opA(Var("x"), Var("y"), "*")),
            `:=`(Var("y"), 0, Expr.opA(Var("y"), Expr.intValue(1), "+"))
          )
        )
      )
    )
  )

  val stmt_report_interval_old = Stmts(
    List(
      Start(),
      `:=`(Var("y"), 0, Expr.intValue(1)),
      While_(
        Expr.opR(Var("y"), Expr.intValue(11), "<="),
        0,
        Stmts(
          List(
            `:=`(Var("z"), 0, Expr.opA(Var("x"), Var("y"), "*")),
            `:=`(Var("y"), 0, Expr.opA(Var("y"), Expr.intValue(1), "+"))
          )
        )
      ),
      End()
    )
  )

  val stmt5_vars_loop_interval = Stmts(
    List(
      Start(),
      `:=`(Var("a"), 0, Var("b")),
      `:=`(Var("b"), 0, Expr.intValue(10)),
      `:=`(Var("c"), 0, Expr.intValue(1000)),
      While_(Expr.T, 0, Stmts(List(`:=`(Var("b"), 0, Expr.opA(Var("b"), Expr.intValue(10), "+"))))),
      End()
    )
  )

  val stmt6_vars_2loop_interval = Stmts(
    List(
      Start(),
      `:=`(Var("a"), 0, Var("b")),
      `:=`(Var("b"), 0, Expr.intValue(10)),
      `:=`(Var("c"), 0, Expr.intValue(1000)),
      While_(
        Expr.T,
        0,
        Stmts(
          List(
            `:=`(Var("b"), 0, Expr.opA(Var("b"), Expr.intValue(10), "+")),
            While_(Expr.T, 0, Stmts(List(`:=`(Var("b"), 0, Expr.opA(Var("b"), Expr.intValue(10), "+")))))
          )
        )
      ),
      End()
    )
  )

  val stmt_rec = Stmts(List(:=(Var("b"), 1, Expr.T), Stmts(List(:=(Var("c"), 2, Expr.T), Stmts(List())))))

  /*
  y:=1;
do x>0 -> y:=x*y;
        x:=x-1
od
   */
  import Expr._
  val stmt_fib = Stmts(
    List(
      `:=`(Var("y"), 0, Expr.intValue(1)),
      While_(
        Expr.opR(Var("x"), intValue(0), ">"),
        0,
        Stmts(List(`:=`(Var("y"), 0, Var("x")), `:=`(Var("x"), 0, Var("x"))))
      )
    )
  )

}
