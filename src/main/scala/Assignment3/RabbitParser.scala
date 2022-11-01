/* --------------------------------------------------------------------------------------- *
 * --------------------------------------------------------------------------------------- *
 *                            EPL ASSIGNMENT 3 - VERSION 1.1                               *
 * --------------------------------------------------------------------------------------- *
 * --------------------------------------------------------------------------------------- */

package Assignment3.RabbitParser


import Assignment3.RabbitSyntax.Syntax._

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

// Parser
  ////////////////////////////////////////////////////////////////////
  // ************************************************************** //
  // *                   DO NOT CHANGE THIS CODE                  * //
  // ************************************************************** //
  ////////////////////////////////////////////////////////////////////
  
class RabbitParser extends StandardTokenParsers with PackratParsers {

  type P[+A] = PackratParser[A]

  def parseStr(input: String): Expr = {
    phrase(expression)(new lexical.Scanner(input)) match {
      case Success(ast, _) => ast
      case e: NoSuccess => sys.error(e.msg)
    }
  }

  def parse(input: String): Expr = {
    val source = scala.io.Source.fromFile(input)
    val lines = try source.mkString finally source.close()
    parseStr(lines)
  }

  lexical.reserved += ("let", "in", "rec", "if", "then", "else",
    "int", "bool", "string", "true", "false", "fun",
    "case", "list", "ref",
    "fst", "snd", "unit",
    "time", "pure", "read", "moveXY", "blank", "when", "signal", "over", "frame"
  )
  lexical.delimiters += ("=","*", "\\", "+", "-", "(", ")", "==", ":", ":=", ".",
    "->", "=>", "{", "}", "|", "::", "[", "]", ">", "<", "!", "()", "/", ",", "<*>", "<+>", "%"
  )


  // Order of precedence (loosest binding first)
  // <*>
  // <+>
  // ::
  // <, ==, >
  // +, -
  // *
  // !
  lazy val binaryOp: P[Expr] = {
    applicative
  }

  lazy val applicative: P[Expr] = {
    expression~"<*>"~over ^^ {
      case e1~"<*>"~e2 => Apply(e1, e2)
    } | over
  }
  
  lazy val over: P[Expr] = {
    expression~"<+>"~cons ^^ {
      case e1~"<+>"~e2 => Over(e1, e2)
    } | cons
  }

  lazy val cons: P[Expr] = {
    (expression ~ "::" ~ expression) ^^ {
      case e1~"::"~e2 => Cons(e1, e2)
    } | binRel
  }

  lazy val binRel: P[Expr] =
    expression ~ "==" ~ summation ^^ {
      case e1~"=="~e2 => Eq(e1,e2)
    } | expression ~ "<" ~ summation ^^ {
      case e1~"<"~e2 => LessThan(e1,e2)
    } | expression ~ ">" ~ summation ^^ {
      case e1~">"~e2 => GreaterThan(e1,e2)
    } | summation

  lazy val summation: P[Expr] =
    summation ~ "+" ~ prod ^^ {
      case e1~"+"~e2 => Plus(e1,e2)
    } | summation ~ "-" ~ prod ^^ {
      case e1~"-"~e2 => Minus(e1,e2)
    } | prod

  lazy val prod: P[Expr] =
    prod ~ "*" ~ unaryOp ^^ {
      case e1~"*"~e2 => Times(e1,e2)
    } | prod ~ "/" ~ unaryOp ^^ {
      case e1~"/"~e2 => Div (e1,e2)
    } | unaryOp

  lazy val lambda: P[Expr] =
    ("\\" ~> ident) ~ (":" ~> typ) ~ ("." ~> expression) ^^ {
      case arg~ty~body => Lambda(arg,ty,body)
    }

  lazy val rec: P[Expr] =
    ("rec" ~> ident) ~
      ("(" ~> ident) ~ (":" ~> typ) ~ ((")" ~ ":") ~> typ) ~
      ("." ~> expression) ^^ {
        case recArg~funArg~funType~recType~body =>
          Rec(recArg,funArg,funType,recType,body)
      }

  lazy val ifExpr: P[Expr] =
    ("if" ~> expression) ~
      ("then" ~> expression) ~
      ("else" ~> expression) ^^ {
        case cond~e1~e2 => IfThenElse(cond,e1,e2)
      }

  lazy val letExpr: P[Expr] =
    ("let" ~> ident) ~ ("=" ~> expression) ~ ("in" ~> expression) ^^ {
      case binder~e1~e2 => Let(binder,e1,e2)
    }

  lazy val letFun: P[Expr] =
    ("let" ~ "fun" ~> ident) ~ ("(" ~> ident) ~
      (":" ~> typ <~ ")") ~ ("=" ~> expression) ~
      ("in" ~> expression) ^^ {
        case fun~binder~ty~e1~e2 => LetFun(fun,binder,ty,e1,e2)
      }

  lazy val letRec: P[Expr] = {
    ("let" ~ "rec" ~> ident) ~ ("(" ~> ident) ~
      (":" ~> typ <~ ")") ~ (":" ~> typ) ~ ("=" ~> expression) ~
      ("in" ~> expression ) ^^ {
        case fun~binder~xty~ty~e1~e2 => LetRec(fun,binder,xty,ty,e1,e2)
      }
    }

  lazy val letPair: P[Expr] =
    ("let" ~ "(") ~> ident ~ ("," ~> ident <~ ")") ~
      ("=" ~> expression) ~ ("in" ~> expression) ^^ {
        case x~y~e1~e2 => LetPair(x,y,e1,e2)
      }

  lazy val emptyList: P[Expr] =
    (("[" ~ "]") ~> ":" ~> typ) ^^ EmptyList

  lazy val emptyListCase: P[Expr] =
    ((("[" ~ "]") ~ "=>") ~> expression)

  lazy val consListCase: P[(Variable, Variable, Expr)] = {
    (ident<~"::")~(ident <~ "=>")~expression ^^ {
      case i1~i2~e => (i1, i2, e)
    }
  }

  lazy val listCase: P[Expr] = {
      (("case"~> expression <~"{")~
        emptyListCase~("|" ~> consListCase <~ "}")) ^^ {
          case scrutinee~ec~cc =>
          cc match {
            case (i1, i2, cc) => ListCase(scrutinee, ec, i1, i2, cc)
          }
      }
  }

  lazy val unaryOp: P[Expr] =
    fact

  lazy val typ: P[Type] =
    funTyp

  lazy val funTyp: P[Type] =
    typ ~ "->" ~ funTyp ^^ {
      case t1~"->"~t2 => FunTy(t1,t2)
    } | pairTyp

  lazy val pairTyp: P[Type] =
    primitiveType ~ "*" ~ pairTyp ^^ {
      case t1~"*"~t2 => PairTy(t1,t2)
    } | listTyp

  lazy val listTyp: P[Type] =
    "list" ~> ("[" ~> typ <~ "]") ^^ {
    case innerTy => ListTy(innerTy)
  } | signalTyp

  lazy val signalTyp: P[Type] =
    "signal" ~> ("[" ~> typ <~ "]") ^^ {
    case innerTy => SignalTy(innerTy)
  } | primitiveType

  lazy val primitiveType: P[Type] =
    "bool" ^^^ BoolTy |
    "int" ^^^ IntTy |
    "frame" ^^^ FrameTy |
    "string" ^^^ StringTy
    "unit" ^^^ UnitTy | "("~>typ<~")"

  lazy val application: P[Expr] = {
    fact ~ fact ^^ {
      case e1~e2 => App(e1,e2)
    }
  }

  lazy val pure: P[Expr] =
    ("pure" ~> fact) ^^ {
      case e => Pure(e)
    }

  lazy val read: P[Expr] =
    ("read" ~> fact) ^^ {
      case e => Read(e)
    }

  lazy val time: P[Expr] = "time" ^^^ Time

  lazy val blank: P[Expr] = "blank" ^^^ Blank

  lazy val moveXY: P[Expr] =
    ("moveXY" ~ "(") ~> (expression <~ ",") ~ (expression <~ ",") ~ (expression <~ ")") ^^ {
      case x~y~z => MoveXY(x, y, z)
    }

  lazy val when: P[Expr] =
    ("when" ~ "(") ~> (expression <~ ",") ~ (expression <~ ",") ~ (expression <~ ")") ^^ {
      case x~y~z => When(x, y, z)
    }
  lazy val overp: P[Expr] =
    ("over" ~ "(") ~> (expression <~ ",") ~ (expression <~ ")") ^^ {
      case x~y => Over(x, y)
    }
    
  lazy val rabbitConst: P[Expr] =
    time |
    blank |
    read |
    pure | 
    moveXY |
    when |
    overp

  lazy val signalBlock: P[Expr] =
    ("signal" ~ "{" ~> expression <~ "}") ^^ {
      case x => SignalBlock(x)
    }

  lazy val escape: P[Expr] =
    ("%" ~ "(" ~> expression <~ ")") ^^ {
      case x => Escape(x)
    }
  lazy val expression: P[Expr] = simpleExpr

  lazy val simpleExpr: P[Expr] = (
      lambda |
      rec |
      letExpr |
      letFun |
      letRec |
      letPair |
      ifExpr |
      binaryOp |
      listCase |
      emptyList |
      fact
  )

  lazy val pairLit: P[Expr] =
    "(" ~> expression ~ "," ~ expression <~ ")" ^^ {
      case t1~","~t2 => Pair(t1,t2)
    }

  lazy val pairOp: P[Expr] =
    ("fst" ~ "(") ~> expression <~ ")" ^^ (x => Fst(x)) |
      ("snd" ~ "(") ~> expression <~ ")" ^^ (x => Snd(x))

  lazy val operations: P[Expr] = (
    application |
    pairOp 
  )

  lazy val fact: P[Expr] = (
      operations |
      pairLit |
      (ident ^^ Var) |
      (numericLit ^^ { x => IntV(x.toInt) }) |
      (stringLit ^^ StringV) |
      ("()" ^^^ (UnitV)) |
      ("true" ^^^ BoolV(true)) |
      ("false" ^^^ BoolV(false)) |
      "("~>expression<~")" |
      signalBlock |
      rabbitConst |
      escape
  )
}
