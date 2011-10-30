sealed abstract class Exp 
case class Num(n: Int) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Mul(lhs: Exp, rhs: Exp) extends Exp
case class Id(x: Symbol) extends Exp 

val test0 = Add(Mul(Id('x),Num(2)),Add(Id('y),Id('y)))

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

val evalVisitor = Visitor[Env=>Int]( env=>_, (a,b)=>env=>a(env)+b(env), (a,b)=>env=>a(env)*b(env), x=>env => env(x)) 

def eval2(e: Exp, env: Env) = foldExp(evalVisitor,e)(env) 

abstract class ExpC { def apply[T](v: Visitor[T]) : T } // we call this method "apply" because then 
                                                        // we can use function application syntax

implicit def num(n: Int) : ExpC = new ExpC {  // we use implicits for num and id again to make 
     def apply[T](v: Visitor[T]) = v.num(n) } // building expressions more concise.
implicit def id(x: Symbol) : ExpC = new ExpC {
     def apply[T](v: Visitor[T]) = v.id(x) }
def add(l: ExpC, r:ExpC) : ExpC = new ExpC {
     def apply[T](v: Visitor[T]) = v.add(l(v),r(v)) } // note the indirect recursion here!
def mul(l: ExpC, r:ExpC) : ExpC = new ExpC {
     def apply[T](v: Visitor[T]) = v.mul(l(v),r(v)) } // note the indirect recursion here!

	 
	 