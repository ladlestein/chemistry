package com.nowanswers.chemistry

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

}

object Element {
  implicit def ElementToElementalTerm(element: Element) : ElementalTerm = ElementalTerm(element)
  implicit def ElementToQuantifiedTerm(element: Element) : QuantifiedTerm = QuantifiedTerm(element)
  implicit def ElementalTermToQuantifiedTerm(term: ElementalTerm) : QuantifiedTerm = QuantifiedTerm(term)
  implicit def SubstitutionGroupToQuantifiedTerm(group: SubsitutionGroup) : QuantifiedTerm = QuantifiedTerm(group)
  implicit def FunctionalGroupToQuantifiedTerm(group: FunctionalGroup) : QuantifiedTerm = QuantifiedTerm(group)
//  implicit def FunctionalGroupToFormula(group: FunctionalGroup) : Formula = Formula(List(group))
  implicit def QuantifiedTermToFormula(term: QuantifiedTerm) : Formula = Formula(List(term))
  implicit def FormulaToFunctionalGroup(formula: Formula) : FunctionalGroup = FunctionalGroup(formula.terms: _*)

  implicit def ProductToSubstitutionGroup(things: Tuple1[QuantifiedTerm]) : QuantifiedTerm = SubsitutionGroup(things._1)
  implicit def ProductToSubstitutionGroup(things: (QuantifiedTerm, QuantifiedTerm)) : QuantifiedTerm = SubsitutionGroup(things._1, things._2)
  implicit def ProductToSubstitutionGroup(things: (QuantifiedTerm, QuantifiedTerm, QuantifiedTerm)) : QuantifiedTerm = SubsitutionGroup(things._1, things._2, things._3)
  implicit def ProductToSubstitutionGroup(things: (QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm)) : QuantifiedTerm = SubsitutionGroup(things._1, things._2, things._3, things._4)
  implicit def ProductToSubstitutionGroup(things: (QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm)) : QuantifiedTerm = SubsitutionGroup(things._1, things._2, things._3, things._4, things._5)
  implicit def ProductToSubstitutionGroup(things: (QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm)) : QuantifiedTerm = SubsitutionGroup(things._1, things._2, things._3, things._4, things._5, things._6)
  implicit def ProductToSubstitutionGroup(things: (QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm)) : QuantifiedTerm = SubsitutionGroup(things._1, things._2, things._3, things._4, things._5, things._6, things._7)
  implicit def ProductToSubstitutionGroup(things: (QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm)) : QuantifiedTerm = SubsitutionGroup(things._1, things._2, things._3, things._4, things._5, things._6, things._7, things._8)
  implicit def ProductToSubstitutionGroup(things: (QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm)) : QuantifiedTerm = SubsitutionGroup(things._1, things._2, things._3, things._4, things._5, things._6, things._7, things._8, things._9)
  implicit def ProductToSubstitutionGroup(things: (QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm, QuantifiedTerm)) : QuantifiedTerm = SubsitutionGroup(things._1, things._2, things._3, things._4, things._5, things._6, things._7, things._8, things._9, things._10)

  def H = QuantifiedTerm(ElementalTerm(Element("H")))
  def He = QuantifiedTerm(ElementalTerm(Element("He")))
  def Li = QuantifiedTerm(ElementalTerm(Element("Li")))
  def Be = QuantifiedTerm(ElementalTerm(Element("Be")))
  def B = QuantifiedTerm(ElementalTerm(Element("B")))
  def C = QuantifiedTerm(ElementalTerm(Element("C")))
  def N = QuantifiedTerm(ElementalTerm(Element("N")))
  def O = QuantifiedTerm(ElementalTerm(Element("O")))
  def F = QuantifiedTerm(ElementalTerm(Element("F")))
  def Ne = QuantifiedTerm(ElementalTerm(Element("Ne")))
  def Na = QuantifiedTerm(ElementalTerm(Element("Na")))
  def Mg = QuantifiedTerm(ElementalTerm(Element("Mg")))
  def Al = QuantifiedTerm(ElementalTerm(Element("Al")))
  def Si = QuantifiedTerm(ElementalTerm(Element("Si")))
  def P = QuantifiedTerm(ElementalTerm(Element("P")))
  def S = QuantifiedTerm(ElementalTerm(Element("S")))
  def Cl = QuantifiedTerm(ElementalTerm(Element("Cl")))
  def Ar = QuantifiedTerm(ElementalTerm(Element("Ar")))
  def K = QuantifiedTerm(ElementalTerm(Element("K")))
  def Ca = QuantifiedTerm(ElementalTerm(Element("Ca")))
  def Sc = QuantifiedTerm(ElementalTerm(Element("Sc")))
  def Ti = QuantifiedTerm(ElementalTerm(Element("Ti")))
  def V = QuantifiedTerm(ElementalTerm(Element("V")))
  def Cr = QuantifiedTerm(ElementalTerm(Element("Cr")))
  def Mn = QuantifiedTerm(ElementalTerm(Element("Mn")))
  def Fe = QuantifiedTerm(ElementalTerm(Element("Fe")))
  def Co = QuantifiedTerm(ElementalTerm(Element("Co")))
  def Ni = QuantifiedTerm(ElementalTerm(Element("Ni")))
  def Cu = QuantifiedTerm(ElementalTerm(Element("Cu")))
  def Zn = QuantifiedTerm(ElementalTerm(Element("Zn")))
  def Ga = QuantifiedTerm(ElementalTerm(Element("Ga")))
  def Ge = QuantifiedTerm(ElementalTerm(Element("Ge")))
  def As = QuantifiedTerm(ElementalTerm(Element("As")))
  def Se = QuantifiedTerm(ElementalTerm(Element("Se")))
  def Br = QuantifiedTerm(ElementalTerm(Element("Br")))
  def Kr = QuantifiedTerm(ElementalTerm(Element("Kr")))
  def Rb = QuantifiedTerm(ElementalTerm(Element("Rb")))
  def Sr = QuantifiedTerm(ElementalTerm(Element("Sr")))
  def Y = QuantifiedTerm(ElementalTerm(Element("Y")))
  def Zr = QuantifiedTerm(ElementalTerm(Element("Zr")))
  def Nb = QuantifiedTerm(ElementalTerm(Element("Nb")))
  def Mo = QuantifiedTerm(ElementalTerm(Element("Mo")))
  def Tc = QuantifiedTerm(ElementalTerm(Element("Tc")))
  def Ru = QuantifiedTerm(ElementalTerm(Element("Ru")))
  def Rh = QuantifiedTerm(ElementalTerm(Element("Rh")))
  def Pd = QuantifiedTerm(ElementalTerm(Element("Pd")))
  def Ag = QuantifiedTerm(ElementalTerm(Element("Ag")))
  def Cd = QuantifiedTerm(ElementalTerm(Element("Cd")))
  def In = QuantifiedTerm(ElementalTerm(Element("In")))
  def Sn = QuantifiedTerm(ElementalTerm(Element("Sn")))
  def Sb = QuantifiedTerm(ElementalTerm(Element("Sb")))
  def Te = QuantifiedTerm(ElementalTerm(Element("Te")))
  def I = QuantifiedTerm(ElementalTerm(Element("I")))
  def Xe = QuantifiedTerm(ElementalTerm(Element("Xe")))
  def Cs = QuantifiedTerm(ElementalTerm(Element("Cs")))
  def Ba = QuantifiedTerm(ElementalTerm(Element("Ba")))
  def La = QuantifiedTerm(ElementalTerm(Element("La")))
  def Ce = QuantifiedTerm(ElementalTerm(Element("Ce")))
  def Pr = QuantifiedTerm(ElementalTerm(Element("Pr")))
  def Nd = QuantifiedTerm(ElementalTerm(Element("Nd")))
  def Pm = QuantifiedTerm(ElementalTerm(Element("Pm")))
  def Sm = QuantifiedTerm(ElementalTerm(Element("Sm")))
  def Eu = QuantifiedTerm(ElementalTerm(Element("Eu")))
  def Gd = QuantifiedTerm(ElementalTerm(Element("Gd")))
  def Tb = QuantifiedTerm(ElementalTerm(Element("Tb")))
  def Dy = QuantifiedTerm(ElementalTerm(Element("Dy")))
  def Ho = QuantifiedTerm(ElementalTerm(Element("Ho")))
  def Er = QuantifiedTerm(ElementalTerm(Element("Er")))
  def Tm = QuantifiedTerm(ElementalTerm(Element("Tm")))
  def Yb = QuantifiedTerm(ElementalTerm(Element("Yb")))
  def Lu = QuantifiedTerm(ElementalTerm(Element("Lu")))
  def Hf = QuantifiedTerm(ElementalTerm(Element("Hf")))
  def Ta = QuantifiedTerm(ElementalTerm(Element("Ta")))
  def W = QuantifiedTerm(ElementalTerm(Element("W")))
  def Re = QuantifiedTerm(ElementalTerm(Element("Re")))
  def Os = QuantifiedTerm(ElementalTerm(Element("Os")))
  def Ir = QuantifiedTerm(ElementalTerm(Element("Ir")))
  def Pt = QuantifiedTerm(ElementalTerm(Element("Pt")))
  def Au = QuantifiedTerm(ElementalTerm(Element("Au")))
  def Hg = QuantifiedTerm(ElementalTerm(Element("Hg")))
  def Tl = QuantifiedTerm(ElementalTerm(Element("Tl")))
  def Pb = QuantifiedTerm(ElementalTerm(Element("Pb")))
  def Bi = QuantifiedTerm(ElementalTerm(Element("Bi")))
  def Po = QuantifiedTerm(ElementalTerm(Element("Po")))
  def At = QuantifiedTerm(ElementalTerm(Element("At")))
  def Rn = QuantifiedTerm(ElementalTerm(Element("Rn")))
  def Fr = QuantifiedTerm(ElementalTerm(Element("Fr")))
  def Ra = QuantifiedTerm(ElementalTerm(Element("Ra")))
  def Ac = QuantifiedTerm(ElementalTerm(Element("Ac")))
  def Th = QuantifiedTerm(ElementalTerm(Element("Th")))
  def Pa = QuantifiedTerm(ElementalTerm(Element("Pa")))
  def U = QuantifiedTerm(ElementalTerm(Element("U")))
  def Np = QuantifiedTerm(ElementalTerm(Element("Np")))
  def Pu = QuantifiedTerm(ElementalTerm(Element("Pu")))
  def Am = QuantifiedTerm(ElementalTerm(Element("Am")))
  def Cm = QuantifiedTerm(ElementalTerm(Element("Cm")))
  def Bk = QuantifiedTerm(ElementalTerm(Element("Bk")))
  def Cf = QuantifiedTerm(ElementalTerm(Element("Cf")))
  def Es = QuantifiedTerm(ElementalTerm(Element("Es")))
  def Fm = QuantifiedTerm(ElementalTerm(Element("Fm")))
  def Md = QuantifiedTerm(ElementalTerm(Element("Md")))
  def No = QuantifiedTerm(ElementalTerm(Element("No")))
  def Lr = QuantifiedTerm(ElementalTerm(Element("Lr")))
  def Rf = QuantifiedTerm(ElementalTerm(Element("Rf")))
  def Db = QuantifiedTerm(ElementalTerm(Element("Db")))
  def Sg = QuantifiedTerm(ElementalTerm(Element("Sg")))
  def Bh = QuantifiedTerm(ElementalTerm(Element("Bh")))
  def Hs = QuantifiedTerm(ElementalTerm(Element("Hs")))
  def Mt = QuantifiedTerm(ElementalTerm(Element("Mt")))
  def Ds = QuantifiedTerm(ElementalTerm(Element("Ds")))
  def Rg = QuantifiedTerm(ElementalTerm(Element("Rg")))
  def Cn = QuantifiedTerm(ElementalTerm(Element("Cn")))
  def Uut = QuantifiedTerm(ElementalTerm(Element("Uut")))
  def Uuq = QuantifiedTerm(ElementalTerm(Element("Uuq")))
  def Uup = QuantifiedTerm(ElementalTerm(Element("Uup")))
  def Uuh = QuantifiedTerm(ElementalTerm(Element("Uuh")))
  def Uus = QuantifiedTerm(ElementalTerm(Element("Uus")))
  def Uuo = QuantifiedTerm(ElementalTerm(Element("Uuo")))
  def REE = QuantifiedTerm(ElementalTerm(Element("REE")))

  def `Fe+3` = QuantifiedTerm(ElementalTerm(Element("Fe"), Some(3)))
  def `Mn+2` = QuantifiedTerm(ElementalTerm(Element("Mn"), Some(2)))

}

case class IllegalElementException(symbol: String) extends IllegalArgumentException {
  override def getMessage = "There is no element with symbol " + symbol
}

trait Stoichiometry {
    def complexity: Double
}

case class Element(symbol: String) {
  if (! Elements.raw.contains(symbol)) {
    throw new IllegalElementException(symbol)
  }

  override def toString = symbol
}

case class ElementalTerm(element: Element, charge: Option[Int] = None) extends Term {
  val complexity = 1.0
  def +(value: Int) = ElementalTerm(element, Option(value))

  override def toString = element.toString + (charge match {
    case Some(num) if num != 0 => scala.math.abs(num).toString + (if (num < 0) "-" else "+")
    case _ => ""
  })
}

object ElementalTerm {
  def ElementalTerm(element: Element, charge: Int) = new ElementalTerm(element, Option(charge))
}

case class SubsitutionGroup(terms: QuantifiedTerm*) extends Term {
  val complexity = (terms :\ 0.0) ((term:Stoichiometry, max:Double) => scala.math.max(term.complexity, max))

  override def toString = "(" + (terms mkString ",") + ")"
}

case class FunctionalGroup(terms: QuantifiedTerm*) extends Term{
  val complexity = (terms :\ 0.0) ((term:Stoichiometry, sum:Double) => sum + term.complexity)

  override def toString = "(" + (terms mkString) + ")"
}

trait Term extends Stoichiometry

case class QuantifiedTerm(term: Term, quantity: Double = 1) extends Stoichiometry{
  def complexity = term.complexity * (if (quantity == 1) {1} else {scala.math.sqrt(quantity)})
  def *(value: Int) = QuantifiedTerm(term, quantity * value)
  def +(term: QuantifiedTerm) = Formula(List(this, term))

  override def toString = (term toString) + (quantity match {
    case 1.0 => ""
    case num if num.toInt != num => num toString
    case num => num.toInt toString
  })
}

case class Formula(terms: List[QuantifiedTerm], waterQuantity: Int = 0) {
  val termComplexity = (terms :\ 0.0) ((term:Stoichiometry, sum:Double) => sum + term.complexity)
  val waterComplexity = if (waterQuantity == 1) {1} else {scala.math.sqrt(waterQuantity)}
  val complexity = termComplexity + waterComplexity

  def +(term: QuantifiedTerm) = Formula(terms :+ term)

  override def toString = (terms mkString) + (if (waterQuantity != 0) "·" + waterQuantity + "H2O" else "")
}

trait FormulaParserComponent {

  def parser: FormulaParser

  trait FormulaParser {

    def parseFormula(text: String): Either[String, Formula]
    def parseElement(text: String): Either[String, Element]

  }
}

trait BasicFormulaParserComponent extends FormulaParserComponent {

  val parser = new RegexParsers with FormulaParser {

    val INT = """[1-9][0-9]*"""r

    val NEWLINE = """\r?\n"""r

    val SIGN = """[+-]"""r

    val SYMBOL = """(REE)|([A-Z][a-z]?)"""r


    def charge = INT ~ SIGN ^^ {
      case value ~ "-" => -value.toInt
      case value ~ "+" => value.toInt
    }

    def quantifier = INT ^^ {
      int => int.toInt
    }

    def functionalGroupExpression: Parser[List[QuantifiedTerm]] = {
      "(" ~> rep1(quantifiedTerm) <~ ")"
    }

    def substitutionGroupExpression = {
      "(" ~> rep1sep(quantifiedTerm, ",") <~ ")"
    }

    def molecularWater = "·" ~> INT <~ "H2O" ^^ {
      int => int.toInt
    }


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

    def parseFormula(formulaText: String) = parse(formula, formulaText) match {
      case Success(formula, _) => Right(formula)
      case NoSuccess(message, _) => Left(message)
    }

    def parseElement(elementText: String) = parse(element, elementText) match {
      case Success(element, _) => Right(element)
      case NoSuccess(message, _) => Left(message)
    }

  }

}



