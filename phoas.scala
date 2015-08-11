package phoas.basic

trait PreTerm[X]
case class Lam[X](body: X => PreTerm[X]) extends PreTerm[X]
case class App[X](fun: PreTerm[X], arg: PreTerm[X]) extends PreTerm[X]
case class Var[X](name: X) extends PreTerm[X]

object test {

  // has dangling floats:
  val ex1: PreTerm[Float] =
    Lam { x => App(Var(3.14f), Var(x)) }

  // make it parametric
  trait Term { def apply[X]: PreTerm[X] }

  val ex2 = new Term {
    def apply[X] = Lam { x => App(Var(x), Var(x)) }
  }

  val ex3 = new Term {
    def apply[X] = Lam { x => Lam { y =>
      App(Var(y), Var(x))
    }}
  }

  // example counting the number of variables
  def count: Term => Int = t => {
    def aux: PreTerm[Unit] => Int = {
      case Var(())   => 1
      case App(m, n) => aux(m) + aux(n)
      case Lam(f)    => aux(f(()))
    }
    aux(t.apply[Unit])
  }

  println(count(ex3)) //=> 2

}
