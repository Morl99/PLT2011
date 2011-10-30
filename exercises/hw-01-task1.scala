// Imports from lecture

sealed abstract class Exp 
case class Num(n: Int) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Mul(lhs: Exp, rhs: Exp) extends Exp
case class Id(x: Symbol) extends Exp 

implicit def num2exp(n: Int) = Num(n)
implicit def sym2exp(x: Symbol) = Id(x)

type Env = Map[Symbol,Int]

def eval(e: Exp, env: Env) : Int = e match {
  case Num(n) => n
  case Id(x) => env(x)
  case Add(l,r) => eval(l,env) + eval(r,env)
  case Mul(l,r) => eval(l,env) * eval(r,env)
}

case class Visitor[T](num: Int => T, add: (T,T)=>T, mul: (T,T)=>T, id: Symbol=>T)

def foldExp[T](v: Visitor[T], e: Exp) : T = {
  e match {
    case Num(n) => v.num(n)
    case Id(x) => v.id(x)
    case Add(l,r) => v.add(foldExp(v,l),foldExp(v,r))
    case Mul(l,r) => v.mul(foldExp(v,l),foldExp(v,r))
  }
}

//Scala Import
import scala.collection.Set

/*
Task 1: Variables in AE
=======================

Implement a Scala function that computes the set
of variables which occur in an AE program.

  a) with pattern matching

*/

def variables1(e : Exp) : Set[Symbol] = e match { 
  case Num(n) => Set()
  case Id(x) => Set(x)
  case Add(l,r) => variables1(l) ++ variables1(r)
  case Mul(l,r) => variables1(l) ++ variables1(r)
}

/* 
  
  b) using a visitor and the foldExp function

*/

val variablesVisitor = Visitor[Set[Symbol]](
  n    => Set(), 
 (a,b) => a ++ b,
 (a,b) => a ++ b,
  x    => Set(x)
)

def variables2(e : Exp) : Set[Symbol] = foldExp(variablesVisitor, e)

/* Some test cases, feel free to add more: */

assert(variables1('x) == Set('x))
assert(variables1(Add('x, 'y)) == Set('x, 'y))

assert(variables2('x) == Set('x))
assert(variables2(Add('x, 'y)) == Set('x, 'y))