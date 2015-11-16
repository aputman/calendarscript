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
    
    
    lazy val cal: PackratParser[Cal] = 
      (   "calendar"~string~"{"~secform~"}" ^^ {case "calendar"~name~"{"~form~"}" => new CalendarDef(name, form)} 
      )
    
    lazy val secform: PackratParser[SectionForm] = 
      (  "dates"~"{"~dates~"}"~filler ^^ {case "dates"~"{"~d~"}"~f => new SecFormContainsDates(d, f)}
        | filler ^^ {case f => new SecFormWithOutDates(f)}
      )
    
    lazy val dates: PackratParser[Dates] =
      (  "includes"~"("~dateRanges~")"~dates ^^ {case "includes"~"("~dRange~")"~d => new DatesIncludesWithMore(new IncludesDef(dRange), d)}
        | "excludes"~"("~dateRanges~")"~dates ^^ {case "excludes"~"("~dRange~")"~d => new DatesExcludesWithMore(new ExcludesDef(dRange), d)}
        | "includes"~"("~dateRanges~")" ^^ {case "includes"~"("~dRange~")" => new DatesIncludes(new IncludesDef(dRange))}
        | "excludes"~"("~dateRanges~")" ^^ {case "excludes"~"("~dRange~")" => new DatesExcludes(new ExcludesDef(dRange))}
      )
      
    lazy val dateRanges: PackratParser[DateRanges] =
      (  dateRange~","~dateRanges ^^ {case dr~","~rest => new DateRangesMultipleRanges(dr, rest)}
        | dateRange ^^ {case dr => new DateRangesSingleRange(dr)}
      )
    lazy val dateRange: PackratParser[Period] =
      (  string~"-"~string ^^ {case date1~"-"~date2 => new Period(new DateTime(new SimpleDateFormat( "MM/dd/yyyy" ).parse(date1)),new DateTime(new SimpleDateFormat( "MM/dd/yyyy" ).parse(date2))) }
      
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
      (  "event"~string~"{"~times~"}" ^^ {case "event"~name~"{"~times~"}" => new EventDef(name,times,null)})
    
    lazy val times: PackratParser[TimeOptions] =
      (  "times"~"{"~multipletimes~"}" ^^ {case "times"~"{"~multipletimes~"}" => multipletimes } )
         
    lazy val singletime: PackratParser[TimeOption] = 
      (  "weekly"~"("~number~":"~number ~ "-" ~ number~":"~number ~ weekdays~")" ^^ {case "weekly"~"("~h1~":"~m1 ~ "-" ~ h2~":"~m2 ~ weekdays~")" => new WeeklyTimeDef(new net.fortuna.ical4j.model.DateTime(h1*60*60*100 + m1*60*100), new net.fortuna.ical4j.model.DateTime(h2*60*60*100 + m2*60*100), weekdays)})
      
    lazy val multipletimes: PackratParser[TimeOptions] =
      (  singletime~","~multipletimes ^^ {case s~","~m => new TimeOptionsMultiple(s, m)}
      | singletime ^^ {case singletime => new TimeOptionsOne(singletime)})
      
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
    def string: Parser[String] = """(\w+/*)+""".r

    
     
 }