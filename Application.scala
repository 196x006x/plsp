
/*
 * 作成したプログラムのアプリケーションを記述するためのファイル
 */
object Application extends App {

  //test.gt01
  // test.gt02
  //test.gt03
  //test.gt04
  test.gt05

  /*val x1 = Variable("x1")
  val x2 = Variable("x2")

  val dom1 = Domain(Seq(1,2,3))
  val dom2 = Domain(Seq(1,2,3))

  val c1 = Ne(x1,x2)

  val csp = CSP(Seq(x1,x2),Map(x1 -> dom1, x2 -> dom2),Seq(c1))

  csp.toSugar.foreach(println)*/

}

object test extends App{

  def gt01 = {
    val x1 = Variable("x1")
    val x2 = Variable("x2")
    val x3 = Variable("x3")

    val vars = Seq(x1,x2,x3)

    val doms =
      Seq(
        x1 -> Domain(Seq(1, 2, 3)),
        x2 -> Domain(Seq(1, 2, 3)),
        x3 -> Domain(Seq(1, 2, 3))).toMap

    val cons =
      Seq(
        Ne(x1, x2),
        Ne(x1, x3),
        Ne(x2, x3))

    val csp = new CSP(vars, doms, cons)

    val solver = new GT

    val solution = solver.solve(csp)


    if (solution.nonEmpty) {
      println("s SAT")
      println(solution.get)
    } else {
      println("s UNSAT")
    }

  }

  def gt02 = {
    val x1 = Variable("x1")
    val x2 = Variable("x2")
    val x3 = Variable("x3")

    val vars = Seq(x1, x2, x3)

    val doms =
      Seq(
        x1 -> Domain(Seq(1, 2)),
        x2 -> Domain(Seq(1, 2)),
        x3 -> Domain(Seq(1, 2))).toMap

    val cons =
      Seq(
        Ne(x1, x2),
        Ne(x1, x3),
        Ne(x2, x3))

    val csp = new CSP(vars, doms, cons)

    val solver = new GT

    val solution = solver.solve(csp)

    if (solution.nonEmpty) {
      println("s SAT")
      println(solution.get)
    } else {
      println("s UNSAT")
    }
  }

  def gt03 = {
    val csp = cspFactory.fromFile("CspFiles/PLS03.csp")

    val solver = new GT

    val solution = solver.solve(csp)

    if (solution.nonEmpty) {
      println("s SAT")
      println(solution.get)
    } else {
      println("s UNSAT")
    }

  }

  def gt04 = {
    val csp = cspFactory.fromFile("CspFiles/PLS04.csp")

    val solver = new GT

    val solution = solver.solve(csp)

    if (solution.nonEmpty) {
      println("s SAT")
      println(solution.get)
    } else {
      println("s UNSAT")
    }

  }

  def gt05 = {
    val csp = cspFactory.fromFile("CspFiles/Alldiff-NQueen04.csp")

    csp.toSugar.foreach(println)

    val solver = new BT

    val solution = solver.solve(csp)

    if (solution.nonEmpty) {
      println("s SAT")
      println(solution.get)
    } else {
      println("s UNSAT")
    }

  }



}

object plspSolver {
  def main(args: Array[String]): Unit = {

    val id: String = "196x006x" // 学籍番号を書く

    val fileName = args(0)

    println(s"ID: $id")
    println(s"CSP: $fileName")

    val csp = cspFactory.fromFile(fileName)

    println("c Parse Done.")

    val solver: CspSolver = new BT // new "自分ソルバークラス" を書く
    val solution = solver.solve(csp)
    if (solution.nonEmpty) {
      println("s SAT")
      printAssignment(solution.get)
    } else {
      println("s UNSAT")
    }

  }

  def printAssignment(a: Assignment) = {
    a.amap.map { case (x, v) => s"v ${x.name} = $v" }.foreach(println)
  }
}