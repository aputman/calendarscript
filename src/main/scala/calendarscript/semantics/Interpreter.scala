package calendarscript

/**
 * @author mvalentine
 */

import calendarscript.ir._
import java.io.File
import scalafx.application.JFXApp

import picolib.maze.Maze
import net.fortuna.ical4j.model._
import net.fortuna.ical4j.model.component._
import net.fortuna.ical4j.model.property._

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
  
  def evalCal(ast: AST): (String, net.fortuna.ical4j.model.Calendar) = {
          
    var periodList:PeriodList = new PeriodList()
    
    ast match {
      case CalendarDef(name: String, sectionForm: SectionForm) => {
        var events = evalSectionForm(sectionForm, periodList.toString())
        
        // now add all the events to a calendar
        var cal = new net.fortuna.ical4j.model.Calendar();
        events.foreach { cal.getComponents().add }
        
        // return the calendar with its name
        return (name, cal)
      }
    }
  }
  
  def evalSectionForm(ast: AST, periodListString: String): List[VEvent] = {
    var periodList = new PeriodList(periodListString)
    
    ast match {
      case SecFormContainsDates(dates: Dates, filler: Filler) => {
        var newPeriodList = evalDates(dates, periodList.toString())
        
        return evalFiller(filler, newPeriodList.toString())
      }
      case SecFormWithOutDates(filler: Filler) => {
        return evalFiller(filler, periodList.toString())
      }
    }
  }
  
  def evalDates(ast: Dates, periodListString: String): PeriodList = {
    var periodList = new PeriodList(periodListString)
    
    ast match {
      case DatesIncludes(includes: Includes) => {
        var addPeriods = evalDateRanges(includes)
        periodList add addPeriods
        periodList.normalise()
        return periodList
      }
      case DatesExcludes(excludes: Excludes) => {
        var subtractPeriods = evalDateRanges(excludes)
        periodList subtract subtractPeriods
        periodList.normalise()
        return periodList
      }
      case DatesIncludesWithMore(includes: Includes, rest: Dates) => {
        var addPeriods = evalDateRanges(includes)
        periodList add addPeriods
        periodList.normalise()
        return evalDates(rest, periodList.toString())
      }
      case DatesExcludesWithMore(excludes: Excludes, rest: Dates) => {
        var subtractPeriods = evalDateRanges(excludes)
        periodList subtract subtractPeriods
        periodList.normalise()
        return evalDates(rest, periodList.toString())
      }
    }
  }
  
  def evalDateRanges(ast: AST): PeriodList = {
    
  }
  
  def evalFiller(ast: AST, periodListString: String): List[VEvent] = {
    var periodList = new PeriodList(periodListString)
    
    ast match {
      case FillerSectionWithMore(section: Section, rest: Filler) => {
        var events = evalSection(section, periodList.toString())
        var otherEvents = evalFiller(rest, periodList.toString())
        
        return events ++ otherEvents
      }
      case FillerEventWithMore(event: Event, rest: Filler) => {
        var events = evalEvent(event, periodList.toString())
        var otherEvents = evalFiller(rest, periodList.toString())
        
        return events ++ otherEvents
      }
      case FillerSection(section: Section) => {
        evalSection(section, periodList.toString())
      }
      case FillerEvent(event: Event) => {
        evalEvent(event, periodList.toString())
      }
    }
  }
  
  def evalSection(ast: AST, periodListString: String): List[VEvent] = {
    var periodList = new PeriodList(periodListString)
    
    ast match {
      case SectionDef(name: String, sectionForm: SectionForm) => {
        evalSectionForm(sectionForm, periodList.toString())
      }
    }
  }
  
  def evalEvent(event: Event, periodListString: String): List[VEvent] = {
    var periodList = new PeriodList(periodListString)
    
    event match {
      case EventDef(name: String, times: TimeOptions, otherFields: ExtraEventFields) => {
        var rdate = new RDate(periodList)
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