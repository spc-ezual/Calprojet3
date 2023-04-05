package fr.istic.cal.while1cons.tests

import org.junit.Test
import org.junit.Assert._

import fr.istic.cal.while1cons.While1cons._
import fr.istic.cal.while1cons._

class TestsProgram {

  @Test
  def Test_While1ConsProgram_1(): Unit = {
    NewVar.reset();
    assertEquals(
      Progr(
        List(Var("X")),
        List(
          Set(Var("Y"), Nl),
          While(
            VarExp("X"),
            List(
              Set(Var("A0"), Hd(VarExp("X"))),
              Set(Var("Y"), Cons(VarExp("A0"), VarExp("Y"))),
              Set(Var("X"), Tl(VarExp("X")))))),
        List(Var("Y"))),
      while1ConsProgr(
        Progr(
          List(Var("X")),
          List(
            Set(Var("Y"), Nl),
            While(
              VarExp("X"),
              List(
                Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
                Set(Var("X"), Tl(VarExp("X")))))),
          List(Var("Y")))))
  }

  @Test
  def Test_prettyPrintProgram_2(): Unit = {
    NewVar.reset();
    assertEquals(
      Progr(
        List(Var("X")),
        List(
          Set(Var("Y"), Nl),
          While(
            VarExp("X"),
            List(
              Set(Var("A0"), Hd(VarExp("X"))),
              Set(Var("Y"), Cons(VarExp("A0"), VarExp("Y"))),
              While(
                VarExp("X"),
                List(
                  Set(Var("A1"), Hd(VarExp("X"))),
                  Set(Var("Y"), Cons(VarExp("A1"), VarExp("Y"))),
                  Set(Var("X"), Tl(VarExp("X"))))),
              Set(Var("X"), Tl(VarExp("X")))))),
        List(Var("Y"))),
      while1ConsProgr(
        Progr(
          List(Var("X")),
          List(
            Set(Var("Y"), Nl),
            While(
              VarExp("X"),
              List(
                Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
                While(
                  VarExp("X"),
                  List(
                    Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
                    Set(Var("X"), Tl(VarExp("X"))))),
                Set(Var("X"), Tl(VarExp("X")))))),
          List(Var("Y")))))
  }

}