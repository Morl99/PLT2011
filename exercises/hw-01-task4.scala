/*
Task 4: De-Bruijn-Indices 
=========================

As we have seen, it is important to understand where each name is
bound. De-Bruijn-indices are an alternative representation of
terms where this binding structure is made explicit. Instead of
using symbols for bound names, it uses numeric indices. The index
#0 stands for the most recently introduced variable. The index #1
stands for the variable introduced before that. And so on. We
don't need symbols in the with expressions, either, because
everything is clear from the indices.

For example, the expression

  with (x = 11) {
    with (y = 2) {x * y} + 
    with (x = 5) {4 * x}
  }

is written as

  with (11) {
    with (2) {#1 * #0} +     // #0 refers to 2, #1 refers to 11
    with (5) {4 * #0}        // #0 refers to 5
  }
  
a) Convert (by hand) the following expression into a form with
   de-Bruijn-indices:
   
  with (42) {
    with (#0 + 1) {
      with (#1 + #0) {
        #2 + #1 + #0
      }
    }
  }

b) De-Bruijn-indices can be used to implement a check for
   alpha-equivalence, that is, to test whether two expressions
   are identical except for the names of variables. Explain how
   that would work.

c) Expressions in de-Bruijn form can be represented in Scala with
   the following case classes:

*/

sealed abstract class ExpDB 
case class NumDB(n: Int) extends ExpDB
case class AddDB(lhs: ExpDB, rhs: ExpDB) extends ExpDB
case class MulDB(lhs: ExpDB, rhs: ExpDB) extends ExpDB
case class IdDB(index : Int) extends ExpDB 
case class WithDB(xdef: ExpDB, body: ExpDB) extends ExpDB

/* 
   Implement a Scala function that converts from WAE expressions
   to ExpDB. 
   
   Hint 1: The local helper function takes a list of variable
   names currently in scope as additional parameter. You can use
   this list to figure out the index to use for a bound variable.
   
   Hint 2: Throw an error if you encounter an unbound variable.
*/

def convert(wae : Exp) : ExpDB = {
  def helper(wae : Exp, names : List[Symbol]) : ExpDB = 
    wae match {
      ...
    }
  
  helper(wae, List())
}

/* A test case, feel free to add more: */

assert(
  convert(With('x, 11, Add(With('y, 2, Mul('x, 'y)), With('x, 5, Mul(4, 'x))))) ==
  WithDB(11, AddDB(WithDB(2, MulDB(IdDB(1), IdDB(0))), WithDB(5, MulDB(4, IdDB(0))))))

