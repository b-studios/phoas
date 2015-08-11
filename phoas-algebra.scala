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
}
