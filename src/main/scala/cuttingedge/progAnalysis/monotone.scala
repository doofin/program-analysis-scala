package cuttingedge.progAnalysis

import cuttingedge.progAnalysis.ast.Expr.Var
import cuttingedge.progAnalysis.ast._
import cuttingedge.progAnalysis
import cuttingedge.progAnalysis.wlAlgo.WlStack
import cuttingedge.progAnalysis.flowGraph._
import scala.collection.{immutable, mutable}

/** non distributive and non bit vec examples (transfer function decomposable as kill,gen)
  * domain,transfer function,lub,subOrder
  * Interval,Detection of Signs,
  *
  * */
object monotone {

  type Interval    = Option[(Double, Double)]
  type IntervalMap = Map[Var, Interval] // add bottom type mutable.Map[Var, Interval]

  //    todo should be fine because every var is inited with a range
  def m_isBottom(id: IntervalMap) = {
    id.map(x => x._2.isEmpty).forall(x => x)
    false
  }

  val intervalMapSubOrder = { (im1: IntervalMap, im2: IntervalMap) =>
    val bool = im1 forall { k1 =>
      // im1 is subset of im2
      val i1o = k1._2
      val i2o = im2(k1._1)
      val r = for {
        i1 <- i1o
        i2 <- i2o
      } yield IntervalLessEquThan(i1, i2)
      r.getOrElse(i1o.isEmpty)
    }
    /*
    encloseDebug(s"im1 suboderOf im2 ? ${bool}") {
      pp(im1)
      pp(im2)
    }
     */
    bool
  }

  /**least upper bound operator*/
  val intervalLUB: (Interval, Interval) => Interval = { (i1: Interval, i2: Interval) =>
    (i1, i2) match {
      case (None, Some(x)) => Option(x)
      case (Some(x), None) => Option(x)
      case (Some(x), Some(y)) =>
        val a = List(x._1, y._1).min
        val b = List(x._2, y._2).max
        Option((a, b))
      case (None, None) => None
    }
  }

  /**least upper bound operator*/
  val IntervalMapLUB = { (i1: IntervalMap, i2: IntervalMap) =>
    /* U( Y )(x) =U {σ (x) | σ ∈ Y} ,PAaA p106 */
    val newmap: Set[(Var, Option[(Double, Double)])] = (i1.keys ++ i2.keys).toSet map { k: Var =>
      val i1o = i1(k)
      val i2o = i2(k)
      val rr  = intervalLUB(i1o, i2o)
      (k, rr)
    }
    val r = Map(newmap.toSeq: _*)
//    pp(r, "lupOp")
    r
  }

  def IntervalAnalysisProgGr(min_max: (Double, Double)): (Stmt, IntervalMap) => IntervalMap = {

    def S_IA(st: Stmt, imM: IntervalMap): IntervalMap = st match {
      case s @ Stmt.`:=`(nm, ln, a) =>
        // should not change this,use immutable!
        imM.updated(nm, A_IA(min_max)(a)(imM))

      case bool: Expr_b =>
//        for booleans
        val bSet   = basicIntervalMap(min_max)(imM)
        val bSetOk = bSet.filter(x => B_IA(min_max)(bool)(x) contains true)
//          pp(bSet, "bSet")
//          pp(bSetOk, "bSetOk")
        val combined = if (bSetOk.isEmpty) imM else bSetOk.reduce(IntervalMapLUB)

//        println(s"bset empty for ${bool}")
        combined
      case _ => imM
    }

    S_IA
  }

  /** all combinations of segments of var assignment
    * deal with multiple vars? use flatmap to concat*/
  def basicIntervalMap(min_max: (Double, Double))(xs: IntervalMap): List[Map[Var, Interval]] = {
    val (min, max) = min_max

    def base(iv: Interval): Set[Interval] = {
      val rr = iv match {
        case Some((a, b)) =>
          assert(a <= b)
          val boolTup = (min <= a, a <= max, min <= b, b <= max)

          val boolTup2 = (IntervalWithin(a, min_max), IntervalWithin(b, min_max))

          val r = boolTup match {
            case (true, true, true, true) =>
//              (a, b) within min,max
              Set((a, b)) //(a, b)
            case (true, true, _, false) =>
//             a is in minmax ,b isn't (0.0,Infinity)
              Set((a, max), (max, b)) //(-Infinity,Infinity)
            case (false, _, true, true) =>
              //             a isn't in minmax ,b is  (0.0,Infinity)
              Set((a, min), (min, b)) //(-Infinity,Infinity)

            case (false, _, _, false) =>
              //  a,b contains minmax             (-Infinity,Infinity) ,
              Set((a, min), (min, max), (max, b)) //Set((min, a), (a, b), (b, max))
            case x =>
//              todo : shouldn't happen fix this
              assert(false, s"shouldn't happen this case ${x}")
              Set((min, a), (a, b), (b, max)) //Set(min_max)
          }
//          println(s"bool tup $boolTup,iv ${iv} decompose to ${r}")
          r
        case None => Set()
      }

      rr map (x => Option(x))
    }

    val r3 = for {
      (nm, iv) <- xs.toSet
      biv      <- base(iv)
    } yield { (nm, biv) }

//    pp(r3, "r3 : ")
    val finalR = r3.toSeq.combinations(3).filter(x => x.length == x.map(_._1).distinct.length).map(_.toMap).toList
//    pp(finalR, "r3 comb : ")

    finalR
  }

  def op_r_<=(a1: (Double, Double), a2: (Double, Double)): Set[Boolean] = {
    val (z_11, z_12) = a1
    val (z_21, z_22) = a2

    val bTup = (z_12 <= z_21, z_12 >= z_22)
    val br = bTup match {
      case (true, _) => Set(true)
      case (_, true) => Set(false)
      case _         => Set(true, false)
    }
//    println(s"op_r_<= : ${a1} <= $a2 ? ${bTup}  ${br}")
    br
  }
  def op_not(x: Set[Boolean]): Set[Boolean] = {
    x.map(!_)
  }

  def op_b_union(a1: Set[Boolean], a2: Set[Boolean], op: (Boolean, Boolean) => Boolean): Set[Boolean] = {
    for {
      a <- a1
      b <- a2
    } yield op(a, b)
  }

  /**check whether m fulfill bool Expr_b*/
  def B_IA(min_max: (Double, Double))(e: Expr_b)(m: IntervalMap): Set[Boolean] = {
    val (min, max) = min_max

    e match {
      case Expr.T => Set(true)
      case Expr.Not(b) =>
        op_not(B_IA(minmax)(b)(m))
      case Expr.opR(a, b, op) =>
        val r = for {
          aa <- A_IA(min_max)(a)(m)
          bb <- A_IA(min_max)(b)(m)
        } yield {
          op match {
            case "<=" =>
              val rr = op_r_<=(aa, bb)
//              println(s"B IA ${(aa, op, bb)} ", rr)
              rr
          }
        }

        r getOrElse Set()
      case Expr.opB(a, b, op) =>
        op match {
          case "" =>
        }
        op_b_union(B_IA(min_max)(a)(m), B_IA(min_max)(a)(m), _ || _)
    }
  }

  def A_IA(min_max: (Double, Double))(e: Expr)(m: IntervalMap): Interval = {
    val (min, max) = min_max
    def op_a_+*(a1: Interval, a2: Interval, op: (Double, Double) => Double): Interval = {
      for {
        (z_11, z_12) <- a1
        (z_21, z_22) <- a2
      } yield {
        val d1 = op(z_11, z_21)
        val z1 = (min <= d1, d1 <= max, z_11.isNegInfinity || z_12.isNegInfinity) match {
          case (true, true, _)              => d1
          case (false, _, _) | (_, _, true) => Double.NegativeInfinity
          case (_, false, _)                => max
        }

        val d2 = op(z_12, z_22)
        val z2 = (min <= d2, d2 <= max, z_21.isPosInfinity || z_22.isPosInfinity) match {
          case (true, true, _)              => d2
          case (false, _, _)                => min
          case (_, false, _) | (_, _, true) => Double.PositiveInfinity
        }

        (z1, z2)
      }

    }

    e match {
      case x: Var => m(x)
      case Expr.intValue(n) =>
        val r: Option[(Double, Double)] = (min <= n, n <= max, m_isBottom(m)) match {
          case (true, true, false) => Some((min, max)) //(n, n)
          case (false, _, false)   => Some((Double.NegativeInfinity, n)) //n<min
          case (_, false, false)   => Some((n, Double.PositiveInfinity)) // n>max
          case (_, _, true)        => None
        }
//        println("Expr.intValue(n)", n, r)
        r

      case x @ Expr.opA(a, b, s) =>
        /*
        val r = for {
          a <- A_IA(a)(m)
          b <- A_IA(b)(m)
        } yield {
          val ir = s match {
            case "+" => op_a_+(a, b)
//            case "*" => op_a_*(a, b)
          }

          println(s"expr : ${x} lift to interval: ${a} $s $b = r:$ir")
          ir
        }
         */

        s match {
          case "+" => op_a_+*(A_IA(min_max)(a)(m), A_IA(min_max)(b)(m), _ + _)
          case "*" => op_a_+*(A_IA(min_max)(a)(m), A_IA(min_max)(b)(m), _ * _)
        }

      case _ => ???
      //      case Expr.opB(a, b, op) => ???
    }
  }

  /** <= for interval wl algo.*/
  def IntervalLessEquThan(a1: (Double, Double), a2: (Double, Double)): Boolean = {

    val (z_11, z_12) = a1
    val (z_21, z_22) = a2

//    a1<=a2,
    z_11 >= z_21 && z_12 <= z_22
  }

  /** <= for interval wl algo.*/
  def IntervalWithin(a1: Double, b: (Double, Double)): Boolean = {
    a1 >= b._1 && a1 <= b._2
  }

  private val minmax: (Double, Double) = (0, 10)

  def test = {
    intervalProgGrTest
  }
  def testBool = {
    val varlist = "xyz" //"abc"
    val initVars: IntervalMap =
      Map(
        varlist.toCharArray
          .map(x => Var(x.toString) -> Option((Double.NegativeInfinity, Double.PositiveInfinity)))
          .toList: _*
      )
    println(basicIntervalMap(minmax)(initVars))
  }

  def intervalProgGrTest = {

    val rg = (Double.NegativeInfinity, Double.PositiveInfinity)
//    val rg = (-1d, 5d)

    val varlist = "xyz" //"abc"
    val initVars: IntervalMap =
      Map(
        varlist.toCharArray
          .map(x => Var(x.toString) -> Option(rg))
          .toList: _*
      )

    val IntervalDomainBottom: IntervalMap =
      Map(
        varlist.toCharArray
          .map(x => Var(x.toString) -> None)
          .toList: _*
      )

    val r = wlAlgo.wlAlgoProgGraph[IntervalMap](
      new WlStack[Int](mutable.ArrayStack()),
      microC_examples.stmt_report_interval,
      IntervalAnalysisProgGr(min_max = minmax),
      subOrderOp = intervalMapSubOrder,
      lubOp = IntervalMapLUB,
      initD = initVars,
      bottomD = IntervalDomainBottom
    )
    pp(r)
  }

  def IntervalAnalysisFlowGr(min_max: (Double, Double)): (Stmt, IntervalMap) => IntervalMap = {
    def S_IA(s: Stmt, imM: IntervalMap): IntervalMap = s match {
      case s @ Stmt.`:=`(nm, ln, a) =>
        val r = A_IA(min_max)(a)(imM)
        println(s"S_IA ${ln}  $s", imM(nm))
        imM.updated(nm, r)

      case Expr.loc(e, l) =>
        //        boolean not work,use prog graph
        e match {
          //          case x: Expr.opB =>
          //            println(x)
          //            assert(false)
          case x: Expr.opR =>
            println(x)
            imM
          case _ =>
            imM

        }

      case _ => imM
    }

    S_IA
  }

  def intervalFlowGrTest = {
//    val s = Stmt.stmt4_vars_interval
//    val s = Stmt.stmt5_vars_loop_interval
    val s = microC_examples.stmt_report_interval
    //    pprintln(s, width = 5)
    val numedStmt = numStmt(s, 0)
    pp(numedStmt, "numedStmt")
    pp(flowG(numedStmt), "flowG")
    pp(blocks(numedStmt), "blocks(numedStmt)")
    val varlist = "xyz" //"abc"
    val initVars: IntervalMap =
      Map(
        varlist.toCharArray
          .map(x => Var(x.toString) -> Option((Double.NegativeInfinity, Double.PositiveInfinity)))
          .toList: _*
      )

    val IntervalDomainBottom: IntervalMap =
      Map(
        varlist.toCharArray
          .map(x => Var(x.toString) -> None)
          .toList: _*
      )

    val r = wlAlgo.wlAlgoFlowGraph[IntervalMap](
      numedStmt,
      IntervalAnalysisFlowGr(min_max = minmax),
      subOrderOp = intervalMapSubOrder,
      lubOp = IntervalMapLUB,
      initD = initVars,
      bottomD = IntervalDomainBottom
    )

    pp(r, "IntervalAnalysis for Flow graph result:")
  }

  /**Detection of Signs
    * DSDomain : a function impl as a map
    * f = ss(stmt) : DSDomain->DSDomain is monotone
    * initially,DSDomain is like [x → {+}, y → {−}, z → {0}]
    * */
  type Signs = String

  type DSDomain = mutable.Map[Var, Signs]
  def DetectionofSigns = {
    def ss(s: Stmt)(m: DSDomain): DSDomain = {
      s match {
        case expr: Expr          => ???
        case Stmt.Stmts(xs)      => ???
        case Stmt.`:=`(l, ln, a) =>
          //          val signs = m(l)
          m(l) = as(a)(m)
          ???
        case Stmt.Tup(l, a, b)       => ???
        case Stmt.If_(b, ln, s1, s2) => ???
        case Stmt.While_(b, ln, s)   => ???
      }
      m
    }

    def as(e: Expr)(m: DSDomain): Signs = e match {
      case x: Var             => m(x)
      case Expr.opA(a, b, op) => ???
      case Expr.opB(a, b, op) => ???
      case _                  => ???
    }
  }
}
