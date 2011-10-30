//Import from Lecture
sealed abstract class Exp 
case class Num(n: Int) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Mul(lhs: Exp, rhs: Exp) extends Exp
case class Id(x: Symbol) extends Exp 
case class With(x: Symbol, xdef: Exp, body: Exp) extends Exp

implicit def num2exp(n: Int) = Num(n)
implicit def sym2exp(x: Symbol) = Id(x)

//Import Scala
//Mh somehow this does work w/o scala.collection.Set, probably automatically imported?

/*
Task 2: Free variables in WAE
=============================

Implement a Scala function that computes the set
of all free (!) variables of a WAE term. 

*/

def freeVariables(wae : Exp) : Set[Symbol] = wae match {
    case Num(n) => Set()
    case Id(x) => Set(x)
    case Add(l,r) => freeVariables(r) ++ freeVariables(l)
    case Mul(l,r) => freeVariables(r) ++ freeVariables(l)
    //remove the bound variable from the Set
    //But recursively determine the Free Vars for the new introduced context xdef
    case With(x, xdef, body) => freeVariables(body) -- Set('x) ++ freeVariables(xdef) 
   
}

/* Some test cases, feel free to define more: */
val test1 = With('x, 5, Add('x,'x)) //Set()
val test2 = With('x, 5, Add('x, With('x, 3,10))) //Set()
val test3 = With('x, 5, Add('x, With('x, 3,'x))) // Set()
val test4 = With('x, 5, Add('x, With('y, 3,'x))) //Set()
val test5 = With('x, 5, With('x, 'x, 'x)) //Set()
val test6 = With('x, 5, With('x, 'y, 'x)) //Set('y)

assert(freeVariables(Id('x)) == Set('x))
assert(freeVariables(With('x, 5, Add('x, 'y))) == Set('y))
val testSet = Set(test1, test2, test3, test4, test5)
testSet.foreach(s => assert(freeVariables(s) == Set()))
assert(freeVariables(test6) == Set('y))
