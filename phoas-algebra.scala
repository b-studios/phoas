package phoas.algebra

trait PreTerm[X, T] {
  def Lam: (X => T) => T
  def Var: X => T
  def App: (T, T) => T
}

object test {

  trait Term {
    def apply[X, T]: PreTerm[X, T] => T
  }

  def ex2 = new Term {
    def apply[X, T] = alg => { import alg._;
      Lam { x => App(Var(x), Var(x)) }
    }
  }

  def ex3 = new Term {
    def apply[X, T] = alg => { import alg._;
      Lam { x => Lam { y =>
        App(Var(y), Var(x))
      }}
    }
  }

  // example counting the number of variables
  object countAlg extends PreTerm[Unit, Int] {
    def Lam = f => f(())
    def App = (m, n) => m + n
    def Var = _ => 1
  }

  def id: Term => Term = t => new Term {
    def apply[X, T] = alg => t.apply(new PreTerm[X, T] {
      def Lam = f => alg.Lam(f)
      def App = (n, m) => alg.App(n, m)
      def Var = x => alg.Var(x)
    })
  }

  println(ex3.apply(countAlg))     // => 2
  println(id(ex3).apply(countAlg)) // => 2


  // X = Location = LValue
  // S = StateTransformers
  // E = State => Value
  trait StmtAlg[X, S, E] {
    def Seq: S => S => S
    def Var: X => E
    def Num: Int => E
    def Let: E => (X => S) => S
    def Assign: X => E => S
  }

  trait StmtTerm {
    def apply[X, S, E]: StmtAlg[X, S, E] => S
  }

  val ex4 = new StmtTerm {
    def apply[X, S, E] = alg => { import alg._;
      Let(Num(0))(x =>
      Let(Num(0))(y =>
        Seq(Assign(x)(Num(1)))
           (Assign(y)(Var(x)))
      ))
    }
  }

  type Location  = Int
  type Value     = Int
  type Count     = Int
  type State     = (Count, Map[Location, Value])

  object stmtSemantics extends StmtAlg[Location, State => State, State => Value] {
    def Seq = s1 => s2 => s1 andThen s2
    def Var = x => { case (_, store) => store(x) }
    def Num = n => _ => n
    def Let = default => f => { case s@(c, store) =>
      val newCount = c + 1
      f(newCount)((newCount, store updated (newCount, default(s))))
    }

    def Assign = x => v => { case s@(c, store) =>
      (c, store updated (x, v(s)))
    }
  }

  println(ex4.apply(stmtSemantics)((0, Map.empty)))

}
