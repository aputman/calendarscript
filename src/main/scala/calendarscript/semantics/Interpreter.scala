package calendarscript

/**
 * @author aputman
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
  
  def evalRCal(ast: AST): (String, net.fortuna.ical4j.model.Calendar) = {
    ast match {
      case RCalWSettings(settings: Settings, cal: Cal) => {
        evalSettings(settings)
        evalCal(cal)
      }
      case RCalWOSettings(cal: Cal) => {
        evalCal(cal)
      }
    }
  }
  
  def evalSettings(ast: Settings) {
    ast match {
      case MultipleSettings(first: Setting, rest: Settings) => {
        evalSetting(first)
        evalSettings(rest)
      }
      case SingleSetting(first: Setting) => {
        evalSetting(first)
      }
    }
  }
  
  // No settings do anything in the interpreter currently, but this has
  // been implemented for ease in future development.
  def evalSetting(ast: Setting) {
    ast match {
      case TimeSetting(rep: String) => {}
      case DateSetting(rep: String) => {}
    }
  }
  
  def evalCal(ast: Cal): (String, net.fortuna.ical4j.model.Calendar) = {
          
    var periodList:PeriodList = new PeriodList()
    
    ast match {
      case CalendarDef(name: String, sectionForm: SectionForm) => {
        var events = evalSectionForm(sectionForm, periodList.toString())
        // now add all the events to a calendar
        var cal = new net.fortuna.ical4j.model.Calendar();
        events.foreach { cal.getComponents() add _ }
        
        // return the calendar with its name
        return (name, cal)
      }
    }
  }
  
  def evalSectionForm(ast: SectionForm, periodListString: String): List[VEvent] = {
    var periodList = new PeriodList(periodListString)
    
    ast match {
      case SecFormContainsDates(dates: Dates, filler: Filler) => {
        var newPeriodList = evalDates(dates, periodList.toString())
        println(newPeriodList)
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
      case DatesIncludes(includes: Includes) => includes match {
        case IncludesDef(dateRanges: DateRanges) => {
          var addPeriods = evalDateRanges(dateRanges)
          periodList = periodList add addPeriods
          periodList.normalise()
          return periodList
        }
      }
      case DatesExcludes(excludes: Excludes) => excludes match {
        case ExcludesDef(dateRanges: DateRanges) => {
          var subtractPeriods = evalDateRanges(dateRanges)
          periodList = periodList subtract subtractPeriods
          periodList.normalise()
          return periodList
        }
      }
      case DatesIncludesWithMore(includes: Includes, rest: Dates) => includes match {
        case IncludesDef(dateRanges: DateRanges) => {
          var addPeriods = evalDateRanges(dateRanges)
          periodList = periodList add addPeriods
          periodList.normalise()
          return evalDates(rest, periodList.toString())
        }
      }
      case DatesExcludesWithMore(excludes: Excludes, rest: Dates) => excludes match {
        case ExcludesDef(dateRanges: DateRanges) => {
          var subtractPeriods = evalDateRanges(dateRanges)
          periodList = periodList subtract subtractPeriods
          periodList.normalise()
          return evalDates(rest, periodList.toString())
        }
      }
    }
  }
  
  def evalDateRanges(ast: DateRanges): PeriodList = {
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
  
  def evalFiller(ast: Filler, periodListString: String): List[VEvent] = {
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
  
  def evalSection(ast: Section, periodListString: String): List[VEvent] = {
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
      case EventDef(name: String, times: TimeOptions) => {
        var events = ICalHelper.createVEvents(periodList, evalTimes(times))
        events.foreach { x => x.getProperties().add(new Summary(name))}
        events
      }
    }
  }
  
  def evalTimes(ast: TimeOptions): List[(DateTime, DateTime, Recur)] = {
    ast match {
      case TimeOptionsOne(option: TimeOption) => {
        List(evalTime(option))
      }
      
      case TimeOptionsMultiple(option: TimeOption, rest: TimeOptions) => {
        evalTime(option) :: evalTimes(rest)
      }
    }
  }
  
  def evalTime(ast: TimeOption): (DateTime, DateTime, Recur) = {
    var untilCal = java.util.Calendar.getInstance()
    untilCal.set(2015, java.util.Calendar.DECEMBER, 31); 
    untilCal.set(java.util.Calendar.MILLISECOND, 0); 
    
    ast match {
          case DailyTimeDef(timerange: TimeRange) => timerange match {
            case TimeRangeMultipleTimes(time1: DateTime, time2: DateTime) => {
              var recur = new Recur(Recur.DAILY, untilCal.getTimeInMillis.toInt)
              (time1, time2, recur)
            }
          }
          case WeeklyTimeDef(timerange: TimeRange, weekDates: WeekDays) => timerange match {
            case TimeRangeMultipleTimes(time1: DateTime, time2: DateTime) => {
              var recur = new Recur(Recur.WEEKLY, untilCal.getTimeInMillis.toInt)
              var weekDayList = evalWeekDays(weekDates)
              weekDayList.foreach { recur.getDayList().add(_) }
              (time1, time2, recur)
            }
          }
        }
  }
  
  def evalWeekDays(ast: WeekDays): List[WeekDay] = {
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