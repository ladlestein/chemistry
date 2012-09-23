package com.nowanswers.chemistry

import scala.util.matching.Regex
import util.parsing.combinator.RegexParsers

object Elements {
  
  val raw = """
  H He 
  Li Be B C N O F Ne 
  Na Mg Al Si P S Cl Ar 
  K Ca Sc Ti V Cr Mn Fe Co Ni Cu Zn Ga Ge As Se Br Kr 
  Rb Sr Y Zr Nb Mo Tc Ru Rh Pd Ag Cd In Sn Sb Te I Xe
  Cs Ba La Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb Lu Hf Ta W Re Os Ir Pt Au Hg Tl Pb Bi Po At Rn
  Fr Ra Ac Th Pa U Np Pu Am Cm Bk Cf Es Fm Md No Lr Rf Db Sg Bh Hs Mt Ds Rg Cn Uut Uuq Uup Uuh Uus Uuo
  REE
"""

  val all = new Regex("[A-Za-z]+")
    .findAllIn(raw).map { Element(_) }.toSeq

}

trait Stoichiometry {
    def complexity: Double
}

case class Element(symbol: String)
case class ElementalTerm(element: Element, charge: Option[Int] = None) extends Term {
    val complexity = 1.0
}
case class SubsitutionGroup(terms: List[Term]) extends Term{
    val complexity = (terms :\ 0.0) ((term:Stoichiometry, max:Double) => scala.math.max(term.complexity, max))
}
case class FunctionalGroup(terms: List[QuantifiedTerm]) extends Term{
    val complexity = (terms :\ 0.0) ((term:Stoichiometry, sum:Double) => sum + term.complexity)
}

trait Term extends Stoichiometry

case class QuantifiedTerm(term: Term, quantity: Double = 1) extends Stoichiometry{
    def complexity = term.complexity * (if (quantity == 1) {1} else {scala.math.sqrt(quantity)})
}

case class Formula(terms: List[QuantifiedTerm], waterQuantity: Int = 0) {
    val termComplexity = (terms :\ 0.0) ((term:Stoichiometry, sum:Double) => sum + term.complexity)
    val waterComplexity = if (waterQuantity == 1) {1} else {scala.math.sqrt(waterQuantity)}
    val complexity = termComplexity + waterComplexity
}

trait FormulaParser extends RegexParsers {

  val INT = """[1-9][0-9]*"""r

  val NEWLINE = """\r?\n"""r

  val SIGN = """[+-]"""r

  val SYMBOL = """(REE)|([A-Z][a-z]?)"""r


  def charge: Parser[Int]
  def quantifier: Parser[Int]
  def element: Parser[Element]
  def elementalTerm : Parser[ElementalTerm]
  def substitutionGroup : Parser[SubsitutionGroup]
  def functionalGroup : Parser[FunctionalGroup]
  def term: Parser[Term]
  def quantifiedTerm: Parser[QuantifiedTerm]
  def molecularWater: Parser[Int]
  def formula: Parser[Formula]

}
