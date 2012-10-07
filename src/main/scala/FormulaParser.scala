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

object Element {
  implicit def ElementToElementalTerm(element: Element) : ElementalTerm = ElementalTerm(element)
  implicit def ElementToQuantifiedTerm(element: Element) : QuantifiedTerm = QuantifiedTerm(element)

  def H = Element("H")
  def He = Element("He")
  def Li = Element("Li")
  def Be = Element("Be")
  def B = Element("B")
  def C = Element("C")
  def N = Element("N")
  def O = Element("O")
  def F = Element("F")
  def Ne = Element("Ne")
  def Na = Element("Na")
  def Mg = Element("Mg")
  def Al = Element("Al")
  def Si = Element("Si")
  def P = Element("P")
  def S = Element("S")
  def Cl = Element("Cl")
  def Ar = Element("Ar")
  def K = Element("K")
  def Ca = Element("Ca")
  def Sc = Element("Sc")
  def Ti = Element("Ti")
  def V = Element("V")
  def Cr = Element("Cr")
  def Mn = Element("Mn")
  def Fe = Element("Fe")
  def Co = Element("Co")
  def Ni = Element("Ni")
  def Cu = Element("Cu")
  def Zn = Element("Zn")
  def Ga = Element("Ga")
  def Ge = Element("Ge")
  def As = Element("As")
  def Se = Element("Se")
  def Br = Element("Br")
  def Kr = Element("Kr")
  def Rb = Element("Rb")
  def Sr = Element("Sr")
  def Y = Element("Y")
  def Zr = Element("Zr")
  def Nb = Element("Nb")
  def Mo = Element("Mo")
  def Tc = Element("Tc")
  def Ru = Element("Ru")
  def Rh = Element("Rh")
  def Pd = Element("Pd")
  def Ag = Element("Ag")
  def Cd = Element("Cd")
  def In = Element("In")
  def Sn = Element("Sn")
  def Sb = Element("Sb")
  def Te = Element("Te")
  def I = Element("I")
  def Xe = Element("Xe")
  def Cs = Element("Cs")
  def Ba = Element("Ba")
  def La = Element("La")
  def Ce = Element("Ce")
  def Pr = Element("Pr")
  def Nd = Element("Nd")
  def Pm = Element("Pm")
  def Sm = Element("Sm")
  def Eu = Element("Eu")
  def Gd = Element("Gd")
  def Tb = Element("Tb")
  def Dy = Element("Dy")
  def Ho = Element("Ho")
  def Er = Element("Er")
  def Tm = Element("Tm")
  def Yb = Element("Yb")
  def Lu = Element("Lu")
  def Hf = Element("Hf")
  def Ta = Element("Ta")
  def W = Element("W")
  def Re = Element("Re")
  def Os = Element("Os")
  def Ir = Element("Ir")
  def Pt = Element("Pt")
  def Au = Element("Au")
  def Hg = Element("Hg")
  def Tl = Element("Tl")
  def Pb = Element("Pb")
  def Bi = Element("Bi")
  def Po = Element("Po")
  def At = Element("At")
  def Rn = Element("Rn")
  def Fr = Element("Fr")
  def Ra = Element("Ra")
  def Ac = Element("Ac")
  def Th = Element("Th")
  def Pa = Element("Pa")
  def U = Element("U")
  def Np = Element("Np")
  def Pu = Element("Pu")
  def Am = Element("Am")
  def Cm = Element("Cm")
  def Bk = Element("Bk")
  def Cf = Element("Cf")
  def Es = Element("Es")
  def Fm = Element("Fm")
  def Md = Element("Md")
  def No = Element("No")
  def Lr = Element("Lr")
  def Rf = Element("Rf")
  def Db = Element("Db")
  def Sg = Element("Sg")
  def Bh = Element("Bh")
  def Hs = Element("Hs")
  def Mt = Element("Mt")
  def Ds = Element("Ds")
  def Rg = Element("Rg")
  def Cn = Element("Cn")
  def Uut = Element("Uut")
  def Uuq = Element("Uuq")
  def Uup = Element("Uup")
  def Uuh = Element("Uuh")
  def Uus = Element("Uus")
  def Uuo = Element("Uuo")
  def REE = Element("REE")

}

trait Stoichiometry {
    def complexity: Double
}

case class Element(symbol: String) {
  def +(value: Int) = ElementalTerm(this, Option(value))
  def *(value: Int) = QuantifiedTerm(ElementalTerm(this), value)
  def ~(term: QuantifiedTerm) = FunctionalGroup(this, term)
}

case class ElementalTerm(element: Element, charge: Option[Int] = None) extends Term {
    val complexity = 1.0
}

object ElementalTerm {
  def ElementalTerm(element: Element, charge: Int) = new ElementalTerm(element, Option(charge))
}

case class SubsitutionGroup(terms: Term*) extends Term{
  val complexity = (terms :\ 0.0) ((term:Stoichiometry, max:Double) => scala.math.max(term.complexity, max))
}

case class FunctionalGroup(terms: QuantifiedTerm*) extends Term{
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
  def substitutionGroupExpression: Parser[List[Term]]
  def functionalGroupExpression: Parser[List[QuantifiedTerm]]
  def molecularWater: Parser[Int]

  def element = SYMBOL ^^ {
    x => Element(x.toString)
  }

  def elementalTerm = element ~ (charge ?) ^^ {
    case sym ~ num => ElementalTerm(sym, num)
  }

  def functionalGroup = functionalGroupExpression ^^ {
    terms => FunctionalGroup(terms : _*)
  }

  def substitutionGroup: Parser[SubsitutionGroup] = substitutionGroupExpression ^^ {
    terms => SubsitutionGroup(terms : _*)
  }

  def term = elementalTerm | substitutionGroup | functionalGroup

  def quantifiedTerm = (term ~ (quantifier ?)) ^^ {
    case t ~ q => QuantifiedTerm(t, q.getOrElse(1).asInstanceOf[Double])
  }

  def formula = (rep1(quantifiedTerm) ~ (molecularWater ?)) ^^ {
    case terms ~ q => Formula(terms, q.getOrElse(0))
  }

  def parseFormula(formulaText: String) = parse(formula, formulaText)

  def parseElement(elementText: String) = parse(element, elementText)



}

