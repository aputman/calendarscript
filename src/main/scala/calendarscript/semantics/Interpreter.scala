package calendarscript

/**
 * @author mvalentine
 */

import calendarscript.ir._
import java.io.File
import scalafx.application.JFXApp
import calendarscript.semantics._
import net.fortuna.ical4j.model._
import net.fortuna.ical4j.model.component._
import net.fortuna.ical4j.model.property._

class interpreter{
  
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
    ast match {
      case DateRangesSingleRange(dateRange: Period) => {
        var pList = new PeriodList()
        pList add dateRange
        pList normalise
      }
      case DateRangesMultipleRanges(dateRange: Period, rest: DateRanges) => {
        var pList = evalDateRanges(rest)
        pList add dateRange
        pList normalise
      }
    }
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
        var evalTimes = List[(DateTime, DateTime, Recur)]()
        ICalHelper.createVEvents(periodList, evalTimes)
      }
    }
  }
  
  def evalTimes(ast: AST): List[(DateTime, DateTime, Recur)] = {
    ast match {
      case TimeOptionsOne(option: TimeOption) => {
        List(evalTime(option))
      }
      
      case TimeOptionsMultiple(option: TimeOption, rest: TimeOptions) => {
        evalTime(option) :: evalTimes(rest)
      }
    }
  }
  
  def evalTime(ast: AST): (DateTime, DateTime, Recur) = {
    ast match {
          case DailyTimeDef(startTime: DateTime, endTime: DateTime) => {
            var recur = new Recur(Recur.DAILY)
            (startTime, endTime, recur)
          }
          case WeeklyTimeDef(startTime: DateTime, endTime: DateTime, weekDates: WeekDays) => {
            var recur = new Recur(Recur.WEEKLY)
            var weekDayList = evalWeekDays(weekDates)
            weekDayList.foreach { recur.getDayList().add(_) }
            (startTime, endTime, recur)
          }
        }
  }
  
  def evalWeekDays(ast: AST): List[WeekDay] = {
    ast match {
      case WeekDaysSingleDay(day: WeekDay) => {
        List(day)
      }
      case WeekDaysMultipleDays(day: WeekDay, rest: WeekDays) => {
        day :: evalWeekDays(rest)
      }
    }
  }
}