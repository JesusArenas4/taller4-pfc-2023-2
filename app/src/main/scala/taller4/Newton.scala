package taller4

class Newton {

  trait Expr
  case class Numero(d: Double) extends Expr
  case class Atomo(x: Char) extends Expr
  case class Suma(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr
  case class Resta(e1: Expr, e2: Expr) extends Expr
  case class Div(e1: Expr, e2: Expr) extends Expr
  case class Expo(e1: Expr, e2: Expr) extends Expr
  case class Logaritmo(e1: Expr) extends Expr


  def evaluar(expr: Expr, valor: Double): Double = expr match {
    case Numero(d) => d
    case Atomo(_) => valor
    case Suma(e1, e2) => evaluar(e1, valor) + evaluar(e2, valor)
    case Prod(e1, e2) => evaluar(e1, valor) * evaluar(e2, valor)
    case Resta(e1, e2) => evaluar(e1, valor) - evaluar(e2, valor)
    case Div(e1, e2) => evaluar(e1, valor) / evaluar(e2, valor)
    case Expo(e1, e2) => math.pow(evaluar(e1, valor), evaluar(e2, valor))
    case Logaritmo(e1) => math.log(evaluar(e1, valor))
  }

  def derivar(expr: Expr, variable: Char): Expr = expr match {
    case Numero(_) => Numero(0)
    case Atomo(x) if x == variable => Numero(1)
    case Atomo(_) => Numero(0)
    case Suma(e1, e2) => Suma(derivar(e1, variable), derivar(e2, variable))
    case Resta(e1, e2) => Resta(derivar(e1, variable), derivar(e2, variable))
    case Prod(e1, e2) => Suma(Prod(derivar(e1, variable), e2), Prod(e1, derivar(e2, variable)))
    case Div(e1, e2) => Div(Resta(Prod(derivar(e1, variable), e2), Prod(e1, derivar(e2, variable))), Prod(e2, e2))
    case Expo(e1, Numero(d)) => Prod(Prod(Numero(d), Expo(e1, Numero(d - 1))), derivar(e1, variable))
    case Logaritmo(e1) => Div(derivar(e1, variable), e1)
  }

  def metodoNewton(expr: Expr, variable: Char, x0: Double, tol: Double, maxIter: Int): Double = {
    def iterar(xi: Double, iter: Int): Double = {
      val fx = evaluar(expr, xi)
      if (math.abs(fx) < tol || iter >= maxIter) {
        xi
      } else {
        val fdx = evaluar(derivar(expr, variable), xi)
        val xi1 = xi - fx / fdx
        iterar(xi1, iter + 1)
      }
    }
    iterar(x0, 0)
  }

  def mostrar(e: Expr): String = e match {
    case Numero(d) => d.toString
    case Atomo(x) => x.toString
    case Suma(e1, e2) => s"(${mostrar(e1)} + ${mostrar(e2)})"
    case Resta(e1, e2) => s"(${mostrar(e1)} - ${mostrar(e2)})"
    case Prod(e1, e2) => s"(${mostrar(e1)} * ${mostrar(e2)})"
    case Div(e1, e2) => s"(${mostrar(e1)} / ${mostrar(e2)})"
    case Expo(e1, e2) => s"(${mostrar(e1)} ^ ${mostrar(e2)})"
    case Logaritmo(e1) => s"(lg(${mostrar(e1)}))"
  }

}
