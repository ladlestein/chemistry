package com.nowanswers.chemistry

import org.specs2.mutable.Specification


class FormulaSpec
  extends Specification {

  import Element._
  "The formula DSL" should {

    "interpret the number after a metal cation as a charge" in {
      `Fe+3` must_== QuantifiedTerm(ElementalTerm((Element("Fe")), Option(3)))
    }

    "interpret the number after * as a quantity" in {
      (Fe * 2) must_== QuantifiedTerm(ElementalTerm(Element("Fe")), 2)
    }

    "interpret addition as a formula" in {
      Cu + O must_== Formula(List(QuantifiedTerm(ElementalTerm(Element("Cu"))), QuantifiedTerm(ElementalTerm(Element("O")))))
    }

    "interpret a tuple as a substitution group" in {
      val expected: QuantifiedTerm = SubsitutionGroup(ElementalTerm(Element("Fe")), ElementalTerm(Element("Ni")))
      val actual: QuantifiedTerm = (Fe, Ni)
      actual must_== expected
    }

//    "interpret a big formula" in {
//      val x = "(Mg,Mn2+,Ca)(Al,Fe3+)(SO4)2F·14H2O"
//      Formula(List((Mg,`Mn+2`,Ca), (Al,`Fe+3`), S + (O*4), F*2), 14) must_== ...
//    }
  }

  "toString()" should {

    "render correctly for an element like Ni" in {
      (Element("Ni") toString) must_== "Ni"
    }

    "render correctly for an elemental term like Fe3+" in {
      (ElementalTerm(Element("Fe"), Option(3)) toString) must_== "Fe3+"
    }

    "render correctly for a quantified term like O2" in {
      (QuantifiedTerm(ElementalTerm(Element("O")), 2) toString) must_== "O2"
    }

    "render correctly for a non-stoichiometric quantified term like Fe3.5" in {
      (QuantifiedTerm(ElementalTerm(Element("Fe")), 3.5) toString) must_== "Fe3.5"
    }

    "render correctly for a functional group like CO3" in {
      (FunctionalGroup(QuantifiedTerm(ElementalTerm(Element("C"))), QuantifiedTerm(ElementalTerm(Element("O")), 3)) toString) must_== "(CO3)"
    }

    "render correctly for a substitution group like (Fe, Mn)" in {
      (SubsitutionGroup(QuantifiedTerm(ElementalTerm(Element("Fe"))), QuantifiedTerm(ElementalTerm(Element("Mn")))) toString) must_== "(Fe,Mn)"
    }

    "render correctly for a formula like Na(OH)" in {
      (Formula(List(
        QuantifiedTerm(ElementalTerm(Element("Na"))),
        QuantifiedTerm(FunctionalGroup(QuantifiedTerm(ElementalTerm(Element("O"))), QuantifiedTerm(ElementalTerm(Element("H")))))))
        toString) must_== "Na(OH)"
    }

    "render correctly for a formula with molecular water like Ca(SO4)·2H2O" in {
      (Formula(List(
        QuantifiedTerm(ElementalTerm(Element("Ca"))),
        QuantifiedTerm(FunctionalGroup(QuantifiedTerm(ElementalTerm(Element("S"))), QuantifiedTerm(ElementalTerm(Element("O")), 4)))
      ), 2) toString) must_== "Ca(SO4)·2H2O"
    }

  }

  "Illegal elements" should {
    "not exist" in {
      Element("Foo") must throwAn[IllegalElementException]
    }
  }


}