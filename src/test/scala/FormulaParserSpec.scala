package com.nowanswers.chemistry

import org.specs2.mutable.Specification


class FormulaParserSpec
  extends Specification with BasicFormulaParserComponent {

  val oxygen = Element("O")
  val hydrogen = Element("H")

  def parseElement(s: String) = parser.parseElement(s) match {
    case Right(element) => element
    case Left(message) => failure message
  }

  def parseFormula(s: String) = parser.parseFormula(s) match {
    case Right(formula) => formula
    case Left(message) => failure message
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
    "parse a formula containing an element with a positive charge" in {
      parseFormula("Fe3+") must_== (Formula(List(QuantifiedTerm(ElementalTerm(Element("Fe"), Option(3))))))
    }
    "parse a formula containing an element with a negative charge" in {
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
      parseFormula("Mn·5H2O") must_== (Formula(List(Mn), 5))
    }

    "parse a big formula" in {
      parseFormula("(Mg,Mn2+,Ca)(Al,Fe3+)(SO4)2F·14H2O") must_== (Formula(
        List(
          QuantifiedTerm(SubsitutionGroup(Mg, `Mn+2`, Ca)),
          QuantifiedTerm(SubsitutionGroup(Al, `Fe+3`)),
          QuantifiedTerm(S+O*4, 2), F), 14)
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