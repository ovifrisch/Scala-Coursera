


class Poly(val terms : Map[Int, Double]) {

  def addTerm(terms: Map[Int, Double], term : (Int, Double)) = {
    val (exp, coef) = term
    terms + (exp -> (coef + terms(exp)))
  }

  def + (other : Poly) = {
    new Poly((other.terms foldLeft terms)(addTerm))
  }
}

