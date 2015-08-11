package phoas.prealgebra

trait PreAlg[X, In, Out] {
  def Lam: (X => Out) => Out
  def Var: X => Out
  def App: (In, In) => Out

  def Num: Int => Out
  def Plus: (In, In) => Out

  def Equals: (In, In) => Out
  def Bool: Boolean => Out
  def If: (In, In, In) => Out
}

trait Term {
  def apply[X, T]: PreAlg[X, T, T] => T
}

object test {


  trait Type
  case object NumT extends Type
  case object BoolT extends Type
  case class TypeVar(n: Int) extends Type
  case class ArrowT(cod: Type, dom: Type) extends Type

  trait Constraint
  case class Eq(fst: Type, snd: Type) extends Constraint

  var _count = 0
  def newTypeVar = {
    _count += 1
    TypeVar(_count)
  }

  // Does not necessarily need a pre-algebra
  // however oa2ag could be used to store the global counter and/or
  // to split the tupled computation in two prealgs.
  object typecheck extends PreAlg[Type, (Type, Set[Constraint]), (Type, Set[Constraint])] {
    def Lam = f => f(newTypeVar)
    def Var = x => (x, Set.empty)
    def App = { case ((t1, cs1), (t2, cs2)) =>
      val v = newTypeVar
      (v, cs1 ++ cs2 + Eq(t1, ArrowT(t2, v)))
    }
    def Num = _ => (NumT, Set.empty)
    def Plus = { case ((t1, cs1), (t2, cs2)) =>
      (NumT, cs1 ++ cs2 + Eq(t1, NumT) + Eq(t2, NumT))
    }
    def Equals = { case ((t1, cs1), (t2, cs2)) =>
      (BoolT, cs1 ++ cs2 + Eq(t1, NumT) + Eq(t2, NumT))
    }
    def Bool = _ => (BoolT, Set.empty)
    def If = { case ((t1, cs1), (t2, cs2), (t3, cs3)) =>
      (t2, cs1 ++ cs2 ++ cs3 + Eq(t2, t3) + Eq(t1, BoolT))
    }
  }

  def ex3 = new Term {
    def apply[X, T] = alg => { import alg._;
      Lam { x => Lam { y =>
        If(Var(x), Var(y), Num(42))
      }}
    }
  }

  // outputs:
  //   type        = TypeVar(2)
  //   constraints = Set(Eq(TypeVar(2),NumT), Eq(TypeVar(1),BoolT)))
  println(ex3.apply(typecheck))

}

