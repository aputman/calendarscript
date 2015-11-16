package calendarscript.semantics


import net.fortuna.ical4j.model._
import net.fortuna.ical4j.model.component._
import net.fortuna.ical4j.model.parameter._
import net.fortuna.ical4j.model.property._

object ICalHelper {
  def createVEvents(periods: PeriodList, times: List[(DateTime, DateTime, Recur)]): List[VEvent] = {
    var events = List[VEvent]()
    println(times)
    times.foreach{
      case (startTime, endTime, recur) => {
        var totalDates:DateList = null
        periods.toArray().foreach{
          case (period: Period) => {
            var newDates = recur.getDates(period.getStart(), period.getEnd(), Value.DATE)
            if (totalDates == null) {
                totalDates = newDates
            }
            else {
              totalDates.addAll(newDates)
            }
          }
        }
        
        var rdate = new RDate(totalDates)
        var newEvent = new VEvent()
        newEvent.getProperties().add(rdate)
        var START = java.util.Calendar.getInstance()
        START.set(2015, java.util.Calendar.DECEMBER, 31, 8, 0, 0); 
        START.set(java.util.Calendar.MILLISECOND, 0); 
        var untilCal = java.util.Calendar.getInstance()
        untilCal.set(2015, java.util.Calendar.DECEMBER, 31, 11, 0, 0); 
        untilCal.set(java.util.Calendar.MILLISECOND, 0); 
        newEvent.getProperties().add(new DtStart(new DateTime(START.getTime()))
        newEvent.getProperties().add(new DtEnd(endTime))
        events = newEvent :: events
      }
    }
    events
  }
}