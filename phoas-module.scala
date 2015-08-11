package phoas.modules

trait PreTerm[X] {
  trait Term
  case class Lam(body: X => Term) extends Term
  case class App(fun: Term, arg: Term) extends Term
  case class Var(name: X) extends Term
}

object test {

  // the only difference between instances of PreTerm[X] is the
  // instantiation of type `X`.

  trait Term {
    def apply[X]: PreTerm[X]#Term
  }

  def ex2 = new Term {
    def apply[X] = { val ts = new PreTerm[X] {}; import ts._;
      Lam { x => App(Var(x), Var(x)) }
    }
  }

  def ex3 = new Term {
    def apply[X] = { val ts = new PreTerm[X] {}; import ts._;
      Lam { x => Lam { y =>
        App(Var(y), Var(x))
      }}
    }
  }
}
