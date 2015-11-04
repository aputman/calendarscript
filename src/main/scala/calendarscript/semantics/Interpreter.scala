package calendarscript

/**
 * @author mvalentine
 */

import calendarscript.ir._
import java.io.File
import scalafx.application.JFXApp

import picolib.maze.Maze

class interpreter extends JFXApp {
  def picobot(mazename: String)(rs: Seq[Rule]*): List[Rule] = {
    val rules = rs.flatten.toList
    
    val maze = Maze("resources" + File.separator + s"${mazename}.txt")
    object RuleBot extends Picobot(maze, rules)
      with TextDisplay with GUIDisplay
    RuleBot.run()
    stage = RuleBot.mainStage
    
    rules
  }
}

package object semantics extends interpreter{
  
  case class RulesDontMakeSense(msg: String) extends Exception(msg)
def eval(ast: AST)(mazeName: String): List[Rule] = {
    val rules = evalTransformers(ast)
    println(rules)
    picobot(mazeName)(rules)
    rules.toList
  }
  
  def evalTransformers(ast: AST): Seq[Rule] = {
    val DefaultSurroundings: Surroundings = Surroundings(Anything, Anything, Anything, Anything)
  
    val UndefinedStateString: String = "!!__ UN DE FI NE D __!!"
    val UndefinedState: State = new State(UndefinedStateString)
    
    val DefaultRule: Rule = Rule(UndefinedState, 
                            DefaultSurroundings, 
                            StayHere, 
                            UndefinedState)
    ast match {
      case BaseTransformer(aug: Augment) => {
        Seq(applyAugment(aug, DefaultRule))
      }
      case AugmentTransformer(aug: Augment, trans: Transformers) =>
          evalTransformers(trans).map {rule => applyAugment(aug, rule)}
          
      case BaseTransformers(trans: Transformer) => evalTransformers(trans)
      case BracedTransformers(multiTrans: MultiTransformers) => evalTransformers(multiTrans)
      
      case SingleMultiTransformers(t: Transformer) => evalTransformers(t)
      case MutlipleMultiTransformers(transL, multiT) => 
        evalTransformers(transL) ++ evalTransformers(multiT)
      case ElseTransformerBasic(Move(dir), trans) => {
        evalTransformers(BaseTransformer(Move(dir))) ++ evalTransformers(AugmentTransformer(Restrict(dir, Blocked), trans))
      }
      case ElseTransformerComplex(Move(dir), trans1, trans2) => {
        evalTransformers(AugmentTransformer(Move(dir), trans1)) ++ evalTransformers(AugmentTransformer(Restrict(dir, Blocked), trans2))
      }
    }
  }
  
  def applyAugment(aug: Augment, rule: Rule): Rule = {
    val UndefinedStateString: String = "!!__ UN DE FI NE D __!!"
    val UndefState = State(UndefinedStateString)
    aug match {
      case Move(dir) => 
        {
          val newRule = rule.copy(moveDirection = dir)
          applyAugment(new Restrict(dir, Open), newRule)
        }
      case Stay() => rule.copy(moveDirection = StayHere)
      case Restrict(dir, relDisc) => 
        val curDir = dir match {
          case North => rule.surroundings.north
          case South => rule.surroundings.south
          case East => rule.surroundings.east
          case West => rule.surroundings.west
        }
        if (curDir != Anything) {
          throw RulesDontMakeSense(s"You tried to set the surroundings in the ${dir} direction twice! Specifically, to ${curDir} and ${dir}.")
        }
        rule.copy(
          surroundings = dir match {
            case North => rule.surroundings.copy(north = relDisc)
            case South => rule.surroundings.copy(south = relDisc)
            case East => rule.surroundings.copy(east = relDisc)
            case West => rule.surroundings.copy(west = relDisc)
          })
      case StateDef(stateName: String) => {
        val newRule = rule.copy(startState = new State(stateName))
        if (newRule.endState == State(UndefinedStateString)) {
          newRule.copy(endState = newRule.startState)
        } else {
          newRule
        }
      }
      case MoveState(stateName: String) => {
        if (rule.endState != State(UndefinedStateString)) {
          throw RulesDontMakeSense(s"You set the end state twice! Specifically, to both ${rule.endState} and ${State(stateName)}.")
        }
        rule.copy(endState = new State(stateName))
      }
    }
}
  
  
}