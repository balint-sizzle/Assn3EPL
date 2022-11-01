/* --------------------------------------------------------------------------------------- *
 * --------------------------------------------------------------------------------------- *
 *                            EPL ASSIGNMENT 3 - VERSION 1.1                               *
 * --------------------------------------------------------------------------------------- *
 * --------------------------------------------------------------------------------------- */

package Assignment3.FRP
import scala.util.DynamicVariable

  ////////////////////////////////////////////////////////////////////
  // ************************************************************** //
  // *                   DO NOT CHANGE THIS CODE                  * //
  // ************************************************************** //
  ////////////////////////////////////////////////////////////////////

// FRP implementation adapted from https://github.com/timo-stoettner/frp-scala
class Signal[T](expr: => T) {
  import Signal._ // Required for 'caller' defined in companion object
  private var curExpr: () => T = _
  private var curVal: T = _
  private var observers: Set[Signal[_]] = Set()

  update(expr)

  protected def computeValue(): Unit = {
    curVal = caller.withValue(this)(curExpr())
    val obs = observers
    observers = Set()
    obs.foreach(_.computeValue())
  }

  protected def update(expr: => T): Unit = {
    curExpr = () => expr
    computeValue()
  }

  def apply() = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    curVal
  }
}

class Var[T](expr: => T) extends Signal[T](expr) {
  override def update(expr: => T): Unit = super.update(expr)
}


// Companion objects to enable instance creation without 'new' keyword
object Signal {
  val caller = new DynamicVariable[Signal[_]](NoSignal)
  def apply[T](expr: => T) = new Signal(expr)

  // Type for time
  type Time = Int
  // Environment time signal
  val envtime: Var[Time] = Var(0)

  // Applicative interfaces for signals
  def pure[A](t: A): Signal[A] = Signal(t)
  def app[A, B](f: Signal[A => B])(t: Signal[A]): Signal[B] = Signal(f()(t()))

  // Helper signal operations
  def lift1[A, B](t: Signal[A])(f: A => B): Signal[B] = app(pure(f))(t)
  def lift2[A, B, C](t1: Signal[A])(t2: Signal[B])(f: A => B => C)
    : Signal[C] = app(app(pure(f))(t1))(t2)
  def lift3[A, B, C, D](t1: Signal[A])(t2: Signal[B])(t3: Signal[C])(f: A => B => C => D)
    : Signal[D] = app(app(app(pure(f))(t1))(t2))(t3)
}
object Var { def apply[T](expr: => T) = new Var(expr) }

object NoSignal extends Signal[Nothing](???) { override def computeValue() = () }
