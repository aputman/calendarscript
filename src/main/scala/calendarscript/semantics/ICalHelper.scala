package calendarscript.semantics


import net.fortuna.ical4j.model._
import net.fortuna.ical4j.model.component._
import net.fortuna.ical4j.model.parameter._
import net.fortuna.ical4j.model.property._

object ICalHelper {
  def createVEvents(periods: PeriodList, times: List[(DateTime, DateTime, Recur)]): List[VEvent] = {
    var events = List[VEvent]()
    var firstDate:DateTime = null
    times.foreach{
      case (startTime, endTime, recur) => {
        var totalDates:DateList = null
        
        periods.toArray().foreach{
          case (period: Period) => {
            var periodStart = period.getStart
            var periodEnd = period.getEnd
            periodStart.setHours(startTime.getHours)
            periodStart.setMinutes(startTime.getMinutes)
            
            var newDates = recur.getDates(periodStart, periodEnd, Value.DATE_TIME)
            if (totalDates == null) {
                totalDates = newDates
            }
            else {
              totalDates.addAll(newDates)
            }
          }
        }
        var rdate = new RDate(totalDates)
        println(rdate)
        var newEvent = new VEvent()
        newEvent.getProperties().add(rdate)
        newEvent.getProperties().add(new DtStart(startTime))
        newEvent.getProperties().add(new DtEnd(endTime))
        events = newEvent :: events
      }
    }
    events
  }
}