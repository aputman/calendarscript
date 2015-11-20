package calendarscript.parser

import scala.util.parsing.combinator._
import java.text.SimpleDateFormat

import calendarscript.ir._
import calendarscript.ir.sugar._

import net.fortuna.ical4j.model._
import net.fortuna.ical4j.model.component._
import net.fortuna.ical4j.model.property._

object CalendarParser extends JavaTokenParsers with PackratParsers {
    // parsing interface
    def apply(s: String): ParseResult[AST] = {
      parseAll(cal, s)
    }
    
    var timeFormat = new SimpleDateFormat("hh:mma");
    var dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    
    lazy val cal: PackratParser[Cal] = 
      (   "calendar"~string~"{"~secform~"}" ^^ {case "calendar"~name~"{"~form~"}" => new CalendarDef(name, form)} 
      )
    
    lazy val secform: PackratParser[SectionForm] = 
      (  "dates"~"{"~dates~"}"~filler ^^ {case "dates"~"{"~d~"}"~f => new SecFormContainsDates(d, f)}
        | filler ^^ {case f => new SecFormWithOutDates(f)}
      )
    
    lazy val dates: PackratParser[Dates] =
      (  "includes"~"{"~dateRanges~"}"~dates ^^ {case "includes"~"{"~dRange~"}"~d => new DatesIncludesWithMore(new IncludesDef(dRange), d)}
        | "excludes"~"{"~dateRanges~"}"~dates ^^ {case "excludes"~"{"~dRange~"}"~d => new DatesExcludesWithMore(new ExcludesDef(dRange), d)}
        | "includes"~"{"~dateRanges~"}" ^^ {case "includes"~"{"~dRange~"}" => new DatesIncludes(new IncludesDef(dRange))}
        | "excludes"~"{"~dateRanges~"}" ^^ {case "excludes"~"{"~dRange~"}" => new DatesExcludes(new ExcludesDef(dRange))}
      )
      
    lazy val dateRanges: PackratParser[DateRanges] =
      (  dateRange~","~dateRanges ^^ {case dr~","~rest => new DateRangesMultipleRanges(dr, rest)}
        | dateRange ^^ {case dr => new DateRangesSingleRange(dr)}
      )
    lazy val dateRange: PackratParser[Period] =
      (  datestring~"-"~datestring ^^ {case date1~"-"~date2 => new Period(new DateTime(dateFormat.parse(date1)), new DateTime(dateFormat.parse(date2))) }
        | datestring ^^ {case date1 => new Period(new DateTime(dateFormat.parse(date1)), new DateTime(dateFormat.parse(date1)))}
      )
    lazy val filler: PackratParser[Filler] =
      (  section~filler ^^ { case sec~f => new FillerSectionWithMore(sec, f) }
         | event~filler ^^ { case ev~f => new FillerEventWithMore(ev, f) }
         | section ^^ { case sec => new FillerSection(sec) }
         | event ^^ { case ev => new FillerEvent(ev) }
      )
      
    lazy val section: PackratParser[Section] =
      (  "section"~string~"{"~secform~"}" ^^ {case "section"~name~"{"~form~"}" => new SectionDef(name, form)} )
    
    lazy val event: PackratParser[Event] = 
      (  "event"~string~"{"~times~"}" ^^ {case "event"~name~"{"~times~"}" => new EventDef(name,times)})
    
    lazy val times: PackratParser[TimeOptions] =
      (  "times"~"{"~multipletimes~"}" ^^ {case "times"~"{"~multipletimes~"}" => multipletimes } )
         
    lazy val singletimeoption: PackratParser[TimeOption] = 
      (  "weekly"~"("~timerange~weekdays~")" ^^ {case "weekly"~"("~timerange~ weekdays~")" => new WeeklyTimeDef(timerange, weekdays)})
      
    lazy val timerange: PackratParser[TimeRange] =
      (  time ~ "-" ~ time ^^ {case  time1 ~ "-" ~ time2 => new TimeRangeMultipleTimes(time1, time2) })
      
    lazy val time: PackratParser[DateTime] =
      (  timestring ^^ {case time1 => new DateTime(timeFormat.parse(time1))})
      
    lazy val multipletimes: PackratParser[TimeOptions] =
      (  singletimeoption~","~multipletimes ^^ {case s~","~m => new TimeOptionsMultiple(s, m)}
      | singletimeoption ^^ {case singletime => new TimeOptionsOne(singletime)})
      
    lazy val weekdays: PackratParser[WeekDays] =
      (  day~","~weekdays ^^ {case d~","~rest => new WeekDaysMultipleDays(d, rest)} 
         | day ^^ {case d => WeekDaysSingleDay(d)})
      
    lazy val day: PackratParser[WeekDay] =
      (  "SU" ^^ {case a => WeekDay.SU} 
         | "MO" ^^ {case a => WeekDay.MO} 
         | "TU" ^^ {case a => WeekDay.TU} 
         | "WE" ^^ {case a => WeekDay.WE} 
         | "TH" ^^ {case a => WeekDay.TH} 
         | "FR" ^^ {case a => WeekDay.FR} 
         | "SA" ^^ {case a => WeekDay.SA}  )
         
    lazy val number: PackratParser[Int] = wholeNumber ^^ { s => s.toInt}
    def string: Parser[String] = """(\w+-*)+""".r
    def datestring: Parser[String] = """(\w+/*)+""".r
    def charstring: Parser[String] = """(\w+:*)+""".r
    def wholenum: Parser[Int] = wholeNumber ^^ {case s => s.toInt}
    def timestring: Parser[String] = """(\w+:*)+""".r
    
     
 }