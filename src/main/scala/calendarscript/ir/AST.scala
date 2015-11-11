package calendarscript.ir

/**
 * @author aputman
 */

import net.fortuna.ical4j.model._

sealed abstract class AST
//sealed abstract class Expr extends AST
sealed abstract class SectionForm extends AST
sealed abstract class Cal extends AST
sealed abstract class Filler extends AST
sealed abstract class Section extends AST
sealed abstract class Dates extends AST
sealed abstract class Includes extends AST
sealed abstract class Excludes extends AST
sealed abstract class DateRanges extends AST
sealed abstract class DateRange extends AST
sealed abstract class Date extends AST
sealed abstract class Event extends AST
sealed abstract class ExtraEventFields extends AST
sealed abstract class Times extends AST
sealed abstract class TimeOptions extends AST
sealed abstract class TimeOption extends AST
sealed abstract class DailyTime extends AST
sealed abstract class WeeklyTime extends AST
sealed abstract class TimeRange extends AST
sealed abstract class Time extends AST
sealed abstract class WeekDays extends AST

// ++
// Top level, every calendarscript must contain a CalendarDef
case class CalendarDef(name: String, sectionForm: SectionForm) extends Cal

// ++
// Every definition of a calendar or section must contain a SectionForm
// If a SectionForm contains dates, they must be at the top of the definition
case class SecFormContainsDates(dates: Dates, filler: Filler) extends SectionForm
case class SecFormWithOutDates(filler: Filler) extends SectionForm

// Filler just means that more sections or events are inside.
case class FillerSectionWithMore(section: Section, rest: Filler) extends Filler
case class FillerEventWithMore(event: Event, rest: Filler) extends Filler
case class FillerSection(section: Section) extends Filler
case class FillerEvent(event: Event) extends Filler

// ++
// Dates are defined by includes and excludes. 
case class DatesIncludesWithMore(includes: Includes, rest: Dates) extends Dates
case class DatesExcludesWithMore(excludes: Excludes, rest: Dates) extends Dates
case class DatesIncludes(includes: Includes) extends Dates
case class DatesExcludes(excludes: Excludes) extends Dates

// Includes and Excludes are just date ranges
case class IncludesDef(dateRanges: DateRanges) extends Includes
case class ExcludesDef(dateRanges: DateRanges) extends Excludes

// By defining DateRanges this precisely, it will allow us to add more variants more easily in the future
case class DateRangesSingleRange(dateRange: Period) extends DateRanges
case class DateRangesMultipleRanges(dateRange: Period, rest: DateRanges) extends DateRanges

// Again, by defining this so specifically, we can add more representation later
// TODO: MAYBE IMPLEMENT LATER
case class DateRangeSingleDate(date: Date) extends DateRange
case class DateRangeMultipleDates(startDate: Date, endDate: Date) extends DateRange

// TODO: ADD IN DATE REPRESENTATION

// Sections act almost identical to calendars
case class SectionDef(name: String, sectionForm: SectionForm) extends Section

// Events must contain a time, can contain other event fields)
case class EventDef(name: String, times: TimeOptions, otherFields: ExtraEventFields) extends Event

case class TimeOptionsOne(option: TimeOption) extends TimeOptions
case class TimeOptionsMultiple(option: TimeOption, rest: TimeOptions) extends TimeOptions

// Time option can be added to, but for now, only daily and weekly
case class TimeOptionDaily(dailyTime: DailyTime) extends TimeOption
case class TimeOptionWeekly(weeklyTime: WeeklyTime) extends TimeOption

// By having these be their own AST defs, we can add more representations later
// FOR NOW, JUST MAKING THESE TIMEOPTIONS
case class DailyTimeDef(startTime: DateTime, endTime: DateTime) extends TimeOption
case class WeeklyTimeDef(startTime: DateTime, endTime: DateTime, weekDates: WeekDays) extends TimeOption

// TODO: MAYBE IMPLEMENT THIS LATER
case class TimeRangeSingleTime(time: Time) extends TimeRange
case class TimeRangeMultipleTimes(startTime: Time, endTime: Time) extends TimeRange

// TODO: ADD IN TIME REPRESENTATION

case class WeekDaysSingleDay(dayName: WeekDay) extends WeekDays
case class WeekDaysMultipleDays(dayName: WeekDay, rest: WeekDays) extends WeekDays

// TODO: ADD IN WEEK DAY NAMES 


