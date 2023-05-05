package fr.istic.cal.while1cons

import scala.util.Try

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
  def while1ConsExprV(expression: Expression): (List[Command], Variable) = {
    expression match{
      case Nl =>
        val nv = NewVar.make()
        (List(Set(nv, Nl)), nv)

      case Cst(name) =>
        val nv = NewVar.make()
        (List(Set(nv, Cst(name))), nv)

      case VarExp(name) =>
        (Nil, Var(name))
        
      case Hd(arg) => 
            val (s1,v1) = while1ConsExprV(arg)
            val nv = NewVar.make()
            val variable = v1 match{
              case Var(name) => VarExp(name)
            }
            (s1 ::: List(Set(nv,Hd(variable))),nv)

      case Tl(arg) => 
            val (s1,v1) = while1ConsExprV(arg)
            val nv = NewVar.make()
            val variable = v1 match{
              case Var(name) => VarExp(name)
            }
            (s1 ::: List(Set(nv,Tl(variable))),nv)

      case Cons(arg1, arg2) =>  
        val (s1,v1) : (List[Command], Variable) = arg1 match{
          case VarExp(name) => (Nil, Var(name))
          case _ => while1ConsExprV(arg1)
        }
        val (s2 , v2) = arg2 match {
          case VarExp(name) => (Nil, Var(name))
          case _ => while1ConsExprV(arg2)
        }
        val variable1 = v1 match{
              case Var(name) => VarExp(name)
            } 
        val variable2 = v2 match{
              case Var(name) => VarExp(name)
            }
        val nv = NewVar.make()
        (s1:::s2:::List(Set(nv,Cons(variable1,variable2))),nv)

      case Eq(arg1, arg2) => 
        val (s1,v1) : (List[Command], Variable) = arg1 match{
          case VarExp(name) => (Nil, Var(name))
          case _ => while1ConsExprV(arg1)
        }
        val (s2 , v2) = arg2 match {
          case VarExp(name) => (Nil, Var(name))
          case _ => while1ConsExprV(arg2)
        }
        val variable1 = v1 match{
              case Var(name) => VarExp(name)
            } 
        val variable2 = v2 match{
              case Var(name) => VarExp(name)
            }
        val nv = NewVar.make()
        (s1:::s2:::List(Set(nv,Eq(variable1,variable2))),nv)
  }
}



  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une paire constituée d'une liste d'affectations ayant le même effet
   * que l'expression et la variable qui contient le résultat
   */
  def while1ConsExprSE(expression: Expression): (List[Command], Expression) = {
    expression match {

      case Hd(arg) => 
        val (s1,v1) = while1ConsExprV(arg)
        val variable1 = v1 match{
              case Var(name) => VarExp(name)
            } 
        (s1,Hd(variable1))

      case Tl(arg) =>  
        val (s1,v1) = while1ConsExprV(arg)
        val variable1 = v1 match{
              case Var(name) => VarExp(name)
            } 
        (s1,Tl(variable1))
      
      case Cons(arg1, arg2) => 
        val (s1,v1) = while1ConsExprV(arg1)
        val (s2,v2) = while1ConsExprV(arg2)
        val variable1 = v1 match{
              case Var(name) => VarExp(name)
            } 
        val variable2 = v2 match{
              case Var(name) => VarExp(name)
            }
        (s1:::s2,Cons(variable1,variable2))


      case Eq(arg1, arg2) => 
        val (s1,v1) = while1ConsExprV(arg1)
        val (s2,v2) = while1ConsExprV(arg2)
        val variable1 = v1 match{
              case Var(name) => VarExp(name)
            } 
        val variable2 = v2 match{
              case Var(name) => VarExp(name)
            }
        (s1:::s2,Eq(variable1,variable2))

      case _ => (Nil,expression)
    }
  }

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
  def while1ConsCommand(command: Command): List[Command] = 
    command match {
      case Nop => List(Nop)

      case Set(variable, expression) => 
        val (s1,v1) = while1ConsExprSE(expression)
        (s1:::List(Set(variable,v1)))
        
      case While(condition, body) => 
        val (s1,Var(v1)) = while1ConsExprV(condition)
        val s2 = while1ConsCommands(body)

        (s1:::List(While(VarExp(v1),s2:::s1)))
      
      case For(condition, body) => 
        val (s1,Var(v1)) = while1ConsExprV(condition)
        val s2 = while1ConsCommands(body)

        (s1:::List(For(VarExp(v1),s2)))

      case If(condition, then_commands, else_commands) => {
        val (s1,Var(v1)) = while1ConsExprV(condition)

        val s2 = while1ConsCommands(then_commands)
        val s3 = while1ConsCommands(else_commands)

        (s1:::List(If(VarExp(v1),s2,s3)))

      }
    }

  /**
   * @param commands : une liste d'AST décrivant une liste de commandes du langage WHILE
   * @return une liste de commandes ayant un seul construteur par expression
   * et ayant le même effet que la commande
   */
  // TODO TP4
  def while1ConsCommands(commands: List[Command]): List[Command] = {
    var rep : List[Command] = List()
    for( command <- commands){
      rep = rep ::: while1ConsCommand(command)
    }
    rep
  }

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @return une liste de chaînes représentant la syntaxe concrète du programme
   */
  // TODO TP4
  def while1ConsProgr(program: Program): Program = {
    program match{
      case Progr(in, body, out) => Progr(in, while1ConsCommands(body), out)
    }
  }

  def main(args: Array[String]): Unit = {
    println(while1ConsExprSE(Eq(Tl(VarExp("Y")),VarExp("X"))))
    println(" ")
    println(while1ConsExprSE(Eq(VarExp("X"), Tl(VarExp("Y")))))
    // vous pouvez ici tester manuellement vos fonctions par des print

  }
}