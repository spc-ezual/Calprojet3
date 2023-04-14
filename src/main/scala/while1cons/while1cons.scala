package fr.istic.cal.while1cons

import scala.util.Try
import javax.lang.model.element.VariableElement

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

object While1cons {

  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s: String): Expression = { WhileParser.analyserexpression(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s: String): Command = { WhileParser.analysercommand(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s: String): Program = { WhileParser.analyserprogram(s) }

  /**
   * UN ELIMINATEUR D'EXPRESSIONS COMPLEXES POUR LE LANGAGE WHILE
   *
   */

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une paire constituée d'une liste d'affectations ayant le même effet
   * que l'expression et une expression qui contient le résultat
   */
  // TODO TP4
  def while1ConsExprV(expression: Expression): (List[Any], Any) = {
    expression match{
      case Nl => {
        val nv:Variable = NewVar.make()
        (List(Set(nv,Nl)),nv)
      }
      case Cst(name) => {
        val nv:Variable = NewVar.make()
        (List(Set(nv,Cst(name))),nv)
      }
      case VarExp(name) => {
        val nv:Variable = NewVar.make()
        (List(Set(nv,VarExp(name))),nv)
      }
      case Hd(arg) => {
        val temp = while1ConsExprV(arg)
        val hd:Variable = NewVar.make()
        val nv:Variable = NewVar.make()
        (temp._1:::List(Set(hd,temp._2),Set(nv,Hd(hd))),nv)
      }
      case Tl(arg) => {
        val temp = while1ConsExprV(arg)
        val tl:Variable = NewVar.make()
        val nv:Variable = NewVar.make()
        (temp._1:::List(Set(tl,temp._2),Set(nv,Tl(tl))),nv)
      }
      case Cons(arg1, arg2) => {
        val temp1 = while1ConsExprV(arg1)
        val temp2 = while1ConsExprV(arg2)
        val t1 : Variable = NewVar.make()
        val t2 : Variable = NewVar.make()
        val nv : Variable = NewVar.make()
        (temp1._1:::temp2._1:::List(Set(t1,temp1._2,Set(t2,temp2._2)),Set(nv,Cons(t1,t2))),nv)
      }
      case Eq(arg1, arg2) => {
        val temp1 = while1ConsExprV(arg1)
        val temp2 = while1ConsExprV(arg2)
        val t1 : Variable = NewVar.make()
        val t2 : Variable = NewVar.make()
        val nv : Variable = NewVar.make()
        (temp1._1:::temp2._1:::List(Set(t1,temp1._2,Set(t2,temp2._2)),Set(nv,Eq(t1,t2))),nv)
      }
    }
  }

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une paire constituée d'une liste d'affectations ayant le même effet
   * que l'expression et la variable qui contient le résultat
   */
  // TODO TP4
  def while1ConsExprSE(expression: Expression): (List[Command], Expression) = ???

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */
  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @return une liste de commandes ayant un seul construteur par expression
   * et ayant le même effet que la commande
   */
  // TODO TP4
  def while1ConsCommand(command: Command): List[Command] = ???

  /**
   * @param commands : une liste d'AST décrivant une liste de commandes du langage WHILE
   * @return une liste de commandes ayant un seul construteur par expression
   * et ayant le même effet que la commande
   */
  // TODO TP4
  def while1ConsCommands(commands: List[Command]): List[Command] = ???

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète du programme
   */
  // TODO TP4
  def while1ConsProgr(program: Program): Program = ???

  def main(args: Array[String]): Unit = {

    // vous pouvez ici tester manuellement vos fonctions par des print

  }
}