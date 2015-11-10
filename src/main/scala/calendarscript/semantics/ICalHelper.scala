package calendarscript.semantics


import net.fortuna.ical4j.model._
import net.fortuna.ical4j.model.component._
import net.fortuna.ical4j.model.parameter._
import net.fortuna.ical4j.model.property._

object ICalHelper {
  def createVEvents(periods: PeriodList, times: List[(DateTime, DateTime, Recur)]): List[VEvent] = {
    var events = List[VEvent]()
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
        newEvent.getProperties().add(new DtStart(startTime))
        newEvent.getProperties().add(new DtEnd(endTime))
        events = newEvent :: events
      }
    }
    events
  }
}