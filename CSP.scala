
/*
 * 制約充足問題 (Constraint Satisfaction Problem; CSP) に関するクラスを定義するためのファイル
 */

abstract class Expression

abstract class Term extends Expression{
  def vars : Set[Variable]
  def valuedWith(a:Assignment):Int
  def toString():String
}

case class Variable(name: String)extends Term{
  def vars = Set(this)
  def valuedWith(a:Assignment) = a(this)
  override def toString() = name
}

//Integer Add not apt

case class Num(value:Int)extends Term{
  def vars = Set.empty[Variable]
  def valuedWith(a:Assignment) = value
  override def toString() = value.toString()
}

case class Add(xs:Seq[Term])extends Term{
  def vars = xs.map(_.vars).reduceLeft((a,b) => a ++ b)
  def valuedWith(a:Assignment) = xs.map(_.valuedWith(a)).sum
  override def toString() = s"(add ${xs.map(_.toString())} )"
}

case class Domain(values: Seq[Int])extends Expression{
  def lb = values.min
  def ub = values.max
  def size = values.size
}

abstract class Constraint extends Expression{
  def vars: Set[Variable]
  def isSatisfiedWith(a:Assignment):Boolean
}

case class CSP(
              var vars: Seq[Variable],
              var doms: Map[Variable,Domain],
              var cons: Seq[Constraint]
              ) {
  def hasNoEmptyDomain = doms.forall(_._2.size > 0)
  def isSatisfiedWith(a: Assignment) = cons.forall(_.isSatisfiedWith(a))

  lazy val var2cons = (for (x <- vars) yield x -> cons.filter(_.vars.contains(x))).toMap

  def toSugar(t: Expression): String = t match {
    case x: Variable => s"(int ${x.name} ${toSugar(doms(x))})"
    case a: Num => s"(int ${a.value.toString()})"
    case t:Add => t.toString()
    case d: Domain => if (d.values.size == d.ub - d.lb + 1) s"${d.lb} ${d.ub}" else d.values.mkString(" ")
    case Ne(x1: Term, x2: Term) => s"(ne ${x1.toString()} ${x2.toString()})"
    case Eq(x1: Term, x2: Term) => s"(eq ${x1.toString()} ${x2.toString()})"
    case Ge(x1: Term, x2: Term) => s"(ge ${x1.toString()} ${x2.toString()})"
    case Alldifferent(xs:Seq[Term]) => s"(alldifferent ${xs.map(_.toString()).toString()})"
  }

  def toSugar: Seq[String] = {
    vars.map(toSugar(_)) ++ cons.map(toSugar(_))
  }
}

object CSP{
  def apply() = new CSP(Seq.empty,Map.empty,Seq.empty)
}

case class Assignment(amap: Map[Variable, Int]){
  def apply(x: Variable) = amap(x)

  def contains(x: Variable) = amap.contains(x)

  def +(x: Variable, v: Int) = Assignment(amap + (x -> v))

  def +(xv: Tuple2[Variable, Int]) = Assignment(amap + (xv._1 -> xv._2))

  def toDoms: Map[Variable, Domain] = amap.map { xv => xv._1 -> Domain(Seq(xv._2)) }

  override def toString = {
    amap.map{case (x, v) => s"v ${x.name} = $v"}.mkString("\n")
  }
}

object Assignment {
  def apply(): Assignment = Assignment(Map.empty)
}

/**
  * x1とx2が異なる値を持つことを表す制約
  * @param x1
  * @param x2
  */
case class Ne(x1: Term, x2:Term)extends Constraint {
  def vars = x1.vars ++ x2.vars
  def isSatisfiedWith(a: Assignment) = x1.valuedWith(a) != x2.valuedWith(a)
}

case class Eq(x1: Term, x2:Term)extends Constraint{
  def vars = x1.vars ++ x2.vars
  def isSatisfiedWith(a:Assignment) = x1.valuedWith(a) == x2.valuedWith(a)
}

case class Ge(x1: Term, x2:Term)extends Constraint{
  def vars = x1.vars ++ x2.vars
  def isSatisfiedWith(a:Assignment) = x1.valuedWith(a) >= x2.valuedWith(a)
}

case class Alldifferent(xs: Seq[Term])extends Constraint{
  def vars = xs.map(_.vars).reduceLeft((a,b) => a ++ b)
  def isSatisfiedWith(a:Assignment) = xs.map(_.valuedWith(a)).distinct == xs.map(_.valuedWith(a))
}


object cspFactory {
  private[this] def varFactory(x: SIntVar): Variable = Variable(x.name)
  private[this] def domFactory(d: SDomain) = {
    val ds = d.dom.foldLeft(Seq.empty[Int])((seq, lu) => seq ++ (lu._1 to lu._2))
    Domain(ds)
  }
  private[this] def termFactory(t: SugarCspTerm): Term = {
    t match {
      case x: SIntVar => varFactory(x)
      case a: SNum => Num(a.n)
      case t: SAdd => Add(t.ts.map(termFactory(_)))
    }
  }
  private[this] def constraintFactory(c: SugarCspConstraint): Constraint = {
    c match {
      case SNe(t1: SugarCspTerm, t2: SugarCspTerm) => Ne(termFactory(t1), termFactory(t2))
      case SEq(t1: SugarCspTerm, t2: SugarCspTerm) => Eq(termFactory(t1), termFactory(t2))
      case SGe(t1: SugarCspTerm, t2: SugarCspTerm) => Ge(termFactory(t1), termFactory(t2))
      case SAlldifferent(ts:Seq[SugarCspTerm]) => Alldifferent(ts.map(termFactory(_)))
    }

  }
  def fromFile(fileName: String): CSP = {
    val csp = CSP()
    val sp = new SugarCspLangParser
    sp.parse(new java.io.File(fileName))
    sp.domMap.keys.foreach { x0 =>
      val x = varFactory(x0)
      csp.vars = x +: csp.vars
      csp.doms += x -> domFactory(sp.domMap(x0))
    }
    csp.cons = sp.scons.map(constraintFactory)
    csp
  }
}