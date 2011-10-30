sealed abstract class Exp 
case class Num(n: Int) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Mul(lhs: Exp, rhs: Exp) extends Exp
case class Id(x: Symbol) extends Exp 
case class With(x: Symbol, xdef: Exp, body: Exp) extends Exp

implicit def num2exp(n: Int) = Num(n)
implicit def sym2exp(x: Symbol) = Id(x)



def makeEval(subst: (Exp,Symbol,Exp)=>Exp) : Exp=>Int = {
  def eval(e: Exp) : Int = e match {
    case Num(n) => n
    case Id(x) => sys.error("unbound variable: "+x)
    case Add(l,r) => eval(l) + eval(r)
    case Mul(l,r) => eval(l) * eval(r)
    case With(x, xdef, body) => eval(subst(body,x,Num(eval(xdef)))) // take the int and wrap it into a Num
  }
  eval
}

 val subst : (Exp,Symbol,Exp) => Exp = (e,i,v) => e match {
    case Num(n) => e
    case Id(x) => if (x == i) v else e
    case Add(l,r) => Add( subst(l,i,v), subst(r,i,v))
    case Mul(l,r) => Mul( subst(l,i,v), subst(r,i,v))
    // handle shadowing correctly
    case With(x,xdef,body) => With(x,
                                   subst(xdef,i,v),
                                   if (x == i) body else subst(body,i,v))
}

val test1 = With('x, 5, Add('x,'x)) //Set()
val test2 = With('x, 5, Add('x, With('x, 3,10))) //Set()
val test3 = With('x, 5, Add('x, With('x, 3,'x))) // Set()
val test4 = With('x, 5, Add('x, With('y, 3,'x))) //Set()
val test5 = With('x, 5, With('x, 'x, 'x)) //Set()
val test6 = With('x, 5, With('x, 'y, 'x)) //Set('y)
