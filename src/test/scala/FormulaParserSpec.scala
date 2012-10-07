package com.nowanswers.chemistry

import org.specs2.mutable.Specification


class FormulaParserSpec
  extends Specification {

  val parser: FormulaParser with Object = new FormulaParser {

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
      "(" ~> rep1sep(term, ",") <~ ")"
    }

    def molecularWater = "·" ~> INT <~ "H2O" ^^ {
      int => int.toInt
    }

  }

  def parseFormula(formulaText: String) = parser.parse(parser.formula, formulaText).get
  def parseElement(elementText: String) = parser.parse(parser.element, elementText).get


  val oxygen = Element("O")
  val hydrogen = Element("H")

  "The formula DSL" should {
    import Element._

    "interpret the number after a metal cation as a charge" in {
      Fe+3 must_== ElementalTerm(Element("Fe"), Option(3))
    }

    "interpret the number after * as a quantity" in {
      (Fe * 2) must_== QuantifiedTerm(ElementalTerm(Element("Fe")), 2)
    }

    "interpret concatenation as a functional group" in {
      Cu ~ O must_== FunctionalGroup(QuantifiedTerm(ElementalTerm(Cu)), QuantifiedTerm(ElementalTerm(O)))
    }
  }

  "The element list" should {
    "not be null" in {
      Elements.all must not beNull
    }
    "contain an element with a single-character symbol" in {
      Elements.all must contain (oxygen)
    }
    "contain an element with a double-character symbol" in {
      Elements.all must contain (Element("Fe"))
    }
    "not contain an element that doesn't exist" in {
      Elements.all must not contain (Element("X"))
    }
  }

  "An element parser" should {
    "parse an element" in {
      parseElement("Fe") must_== (Element("Fe"))
    }
  }

  "A formula parser" should {
    "parse a single-element formula" in {
      parseFormula("Fe") must_== (Formula(List(QuantifiedTerm(ElementalTerm(Element("Fe"))))))
    }
    "parse a formula with a quantified element" in {
      parseFormula("Fe2") must_== (Formula(List(QuantifiedTerm(ElementalTerm(Element("Fe")), 2))))
    }
    "parse a formula with an oxidation number" in {
      parseFormula("Fe3+") must_== (Formula(List(QuantifiedTerm(ElementalTerm(Element("Fe"), Option(3))))))
    }
    "parse a formula with a negative oxidation number" in {
      parseFormula("Br3-") must_== (Formula(List(QuantifiedTerm(ElementalTerm(Element("Br"), Option(-3))))))
    }
    "parse a formula with multiple terms" in {
      parseFormula("OH") must_== (Formula(List(QuantifiedTerm(ElementalTerm(oxygen)), QuantifiedTerm(ElementalTerm(hydrogen)))))
    }
    "parse a formula with a functional group" in {
      parseFormula("H(SO4)") must_== (Formula(List(QuantifiedTerm(ElementalTerm(Element("H"))), QuantifiedTerm(FunctionalGroup(QuantifiedTerm(ElementalTerm(Element("S"))), QuantifiedTerm(ElementalTerm(Element("O")), 4))))))
    }

    import Element._
    "parse a formula with a subsitution group" in {
      parseFormula("(Mn,Fe)") must_== (Formula(List(
        QuantifiedTerm(SubsitutionGroup(Mn,Fe)))))
    }

    "parse a formnula with molecular water" in {
      parseFormula("Mn·5H2O") must_== (Formula(List(
        QuantifiedTerm(Mn)), 5))
    }

    "parse a big formula" in {
      parseFormula("(Mg,Mn2+,Ca)(Al,Fe3+)(SO4)2F·14H2O") must_== (Formula(
        List(
          QuantifiedTerm(SubsitutionGroup(Mg, Mn+2, Ca)),
          QuantifiedTerm(SubsitutionGroup(Al, Fe+3)),
          QuantifiedTerm(S ~ (O*4), 2), F), 14)
      )
    }
  }


  //   def test = {
  //
  //      def check[T](s: String, parser: => Parser[AnyRef], expect: AnyRef): AnyRef = {
  //        println(expect)
  //        val result = parse(parser, s).get
  //        assert(result.toString == expect.toString, "expected: \n" + expect + "; got: \n" + result)
  //        result
  //      }
  //      check("Fe", formula, Formula(List(QuantifiedTerm(ElementalTerm(Element("Fe"))))))
  //      check("Fe_2_", formula, Formula(List(QuantifiedTerm(ElementalTerm(Element("Fe")), 2))))
  //      check("Fe^2+^", formula, Formula(List(QuantifiedTerm(ElementalTerm(Element("Fe"), Option(2))))))
  //      check("Si^4-^", formula, Formula(List(QuantifiedTerm(ElementalTerm(Element("Si"), Option(-4))))))
  //      check("OH", formula, Formula(List(QuantifiedTerm(ElementalTerm(Element("O"))), QuantifiedTerm(ElementalTerm(Element("H"))))))
  //      check("H(SO_4_)", formula, Formula(List(QuantifiedTerm(ElementalTerm(Element("H"))), QuantifiedTerm(FunctionalGroup(List(QuantifiedTerm(ElementalTerm(Element("S"))), QuantifiedTerm(ElementalTerm(Element("O")), 4)))))))
  //
  //    }
  //

}