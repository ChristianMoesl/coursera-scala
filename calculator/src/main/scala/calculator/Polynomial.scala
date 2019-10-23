package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal({
      def f(delta_sqrt: Double): Double = 
        (-b() + delta_sqrt) / 2 * a()

      val d = delta()

      if (d < 0.0) Set()
      else if (d == 1.0) Set(f(1.0))
      else Set(f(Math.sqrt(d)), f(-Math.sqrt(d)))
    })
  }
}
