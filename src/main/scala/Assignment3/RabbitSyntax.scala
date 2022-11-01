/* --------------------------------------------------------------------------------------- *
 * --------------------------------------------------------------------------------------- *
 *                            EPL ASSIGNMENT 3 - VERSION 1.1                               *
 * --------------------------------------------------------------------------------------- *
 * --------------------------------------------------------------------------------------- */

package Assignment3.RabbitSyntax


  ////////////////////////////////////////////////////////////////////
  // ************************************************************** //
  // *                   DO NOT CHANGE THIS CODE                  * //
  // ************************************************************** //
  ////////////////////////////////////////////////////////////////////

object Syntax {
  type Variable = String
  type Env[A] = Map[Variable,A]

  abstract class Expr
  // Arithmetic expressions
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class Minus(e1: Expr, e2: Expr) extends Expr
  case class Times(e1: Expr, e2: Expr) extends Expr
  case class Div(e1: Expr, e2: Expr) extends Expr

  // Booleans
  case class Eq(e1: Expr, e2:Expr) extends Expr
  case class IfThenElse(e: Expr, e1: Expr, e2: Expr) extends Expr
  case class GreaterThan(e1: Expr, e2: Expr) extends Expr
  case class LessThan(e1: Expr, e2: Expr) extends Expr

  // Variables and let-binding
  case class Var(x: Variable) extends Expr
  case class Let(x: Variable, e1: Expr, e2: Expr) extends Expr
  case class LetFun(f: Variable, arg: Variable, ty: Type, e1:Expr, e2:Expr)
      extends Expr
  case class LetRec(f: Variable, arg: Variable, xty: Type, ty: Type, e1:Expr, e2:Expr)
      extends Expr
  case class LetPair(x: Variable, y: Variable, ePair: Expr, eBody: Expr) extends Expr

  // Pairs
  case class Pair(e1: Expr, e2: Expr) extends Expr
  case class Fst(e: Expr) extends Expr
  case class Snd(e: Expr) extends Expr

  // Functions
  case class Lambda(x: Variable, ty: Type, e: Expr) extends Expr
  case class Rec(f: Variable, x: Variable, tyx: Type, ty: Type, e: Expr) extends Expr
  case class App(e1: Expr, e2: Expr) extends Expr

  // Lists
  case class EmptyList(ty: Type) extends Expr
  case class Cons(e: Expr, e2: Expr) extends Expr
  case class ListCase(l: Expr, e1: Expr, x: Variable, y: Variable, e2: Expr) extends Expr

  // Sequencing	       
  case class Seq(e1: Expr, e2: Expr) extends Expr

  // Signals
  case object Time extends Expr
  case class Pure(e: Expr) extends Expr
  case class Apply(e1: Expr, e2: Expr) extends Expr
  case class Read(e: Expr) extends Expr
  case class MoveXY(x: Expr, y: Expr, a: Expr) extends Expr
  case object Blank extends Expr
  case class Over(e1: Expr, e2: Expr) extends Expr
  case class When(e1: Expr, e2: Expr, e3: Expr) extends Expr
  case class SignalBlock(se: Expr) extends Expr
  case class Escape(e: Expr) extends Expr
  
  // Values
  abstract class Value extends Expr
  case object UnitV extends Value
  case class IntV(n: Int) extends Value
  case class BoolV(n: Boolean) extends Value
  case class ListV(l: List[Value]) extends Value
  case class StringV(str: String) extends Value
  case class PairV(fst: Value, snd: Value) extends Value
  case class FunV(x: Variable, ty: Type, e: Expr) extends Value
  case class RecV(f: Variable, x: Variable, tyx: Type, ty: Type, e: Expr) extends Value

  case object TimeV extends Value
  case class PureV(v: Value) extends Value
  case class ApplyV(v1: Value, v2: Value) extends Value
  case class ReadV(v: Value) extends Value
  case class MoveXYV(x: Value, y: Value, a: Value) extends Value
  case object BlankV extends Value
  case class OverV(v1: Value, e2: Value) extends Value
  case class WhenV(v1: Value, v2: Value, v3: Value) extends Value

  // Types
  abstract class Type
  case object UnitTy extends Type
  case object IntTy extends Type
  case object BoolTy extends Type
  case object StringTy extends Type
  case object FrameTy extends Type
  case class ListTy(ty1: Type) extends Type
  case class PairTy(ty1: Type, ty2: Type) extends Type
  case class FunTy(ty1: Type, ty2: Type) extends Type
  case class SignalTy(ty1: Type) extends Type

  object Gensym {
    private var id = 0
    def gensym(s: Variable): Variable = {
      val fresh_s = s + "_" + id
      id = id + 1
      fresh_s
    }
  }
}

