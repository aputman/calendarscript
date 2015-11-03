# calendarscript

Creating complex calendars that can be imported into all popular calendar programs. 


Schedules for most people are static and can be defined by a simple "repeat event"
button. For others, using the simple Google or Apple calendar GUI requires so much
manual adjustment that it becomes almost useless. CalendarScript is for these 
people. 

The idea behind CalendarScript is that all the repetitive events in your life can 
be separated into groups, each of which have more or less the same time line. For
students, one group would be classes, since they all follow the same rules
dictating the dates that school is in session. School semesters don't usually run
from one date to another, they usually contain breaks and holidays on which class
would not meeet. So CalendarScript seeks to give users the greatest expressibility
in defining these rules. 

More simply CalendarScript is designed to allow users to:
+ define weekly or monthy repeated events
+ define sections of events that all follow certain rules (timeline, restrictions, etc)
+ scope these sections to limit the ammount of code written
+ import this calendar into any major calendar program (using icalendar)

---

Here is an example of CalendarScript for a student that only wants a simple schedule
of his classes:

```
calendar school-calendar {
	
	dates {
		includes { 9/1/15 - 12/17/15 }
		excludes { 	10/15/15 - 10/18/15,
					11/25/15 - 11/28/15 }
	}

	section classes {

		dates {
			excludes { 12/9/15 - 12/17/15 }
		}

		event math {
			times { weekly( 10:00am - 11:00am Mon, Wed ) }
		}

		event physics {
			times {	weekly( 11:00am - 12:00pm Mon, Wed) ,
					weekly( 9:00am - 9:50am Tues) }
		}

		event art {
			times { weekly( 1pm - 3pm Tues, Thur) }
		}
	}

	section finals {
		dates {
			excludes { 9/1/15 - 12/9/15 }
		}

		...
	}

	event payBills {
		times { monthly( 2pm - 3pm 30th) }
		type: individual
	}
}
```

The key idea is that events are repeated as for all the possible dates
they could exist. The dates the events can occur on are defined by the 
dates in all the calendars and sections that the event is defined in. 
**Dates defined closer in scope to the event have precedence if date 
ranges don't agree with one another.** This is shown in the example, as
the exclude dates for the class section overlap with the include dates of 
the calendar. Since the class section is closer in scope to each event 
than the caldendar, the exclude dates will take precedence. The dates
in the calendar "includes" that didn't overlap with the excludes will
still be used to define the events. 