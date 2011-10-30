/*

Task 3: Names in Scala
======================

Write three (different, interesting but short) Scala snippets
where a name is shadowed. For each snippet, explain which names
are binding, bound or free; and where the scopes of the binding
names are.

*/

println("Snippet 1")
// x is shadowed inside the pattern matching staement

//binding x
val x = "outerX"
//bound x (outer)
println(x)
"innerX" match {
  //binding a new x
  case x => println(x) //bound x (inner)
} //end of binding name "case x=>"
//bound x (outer)
println(x)


println("Snippet 2")
object Y{
  //binding y
  val y = 1
}
import Y._
//bound y
println(y)
//binding a y in this new scope shadowing Y.y, Y.y is of course still reachable
val y = 2
//bound y (new)
println(y)
println(Y.y)

println("Snippet 3")
abstract class Person { 
  // free Variable name
  val name: String 
}

def fac(name: String) = {
  //bound
   println(name)
   new Person {
     //binding name to inner (scope of Person) 
     val name = "inner"
     //bound (inner scope)
     println(name)
   } //end of Person(inner) scope
   //bound (outer scope)
   println(name)
}
//binding name to "outer"
fac("outer")


