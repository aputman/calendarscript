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

## Running DSL

To run this program, scala and sbt are currently required. Here are the steps 

1. Download and install scala and sbt. 
2. run ```sbt``` in the parent foler of this project.
3. run program (will go into more later) with 2 command line arguments:
  1. the file (including location) of the calendarscript file
  2. the folder location that your resulting ical file will be saved. 

The name of the resulting file will be the name that you gave your calendar. 

---

## Example

Here is an example of CalendarScript for a student that only wants a simple schedule
of his classes:

```
calendar school-calendar {

	section classes {

		dates {
			includes { 1/19/2016 - 5/4/2016 }
			excludes { 	3/11/2016 - 3/20/2016,
						3/25/2016 }
		}
		
		section mudd-classes {
			
			dates {
				excludes { 5/1/2016 - 5/4/2016 }
			}
			event algorithms {
				times { weekly( 11:00am - 12:15pm MO, WE ) }
			}
	
			event interaction-design {
				times {	weekly( 9:35am - 10:50am TU, TH) }
			}
			
			event macroeconomics {
				times { weekly( 2:45pm - 4:00pm MO, WE) }
			}
	
			event clinic {
				times { weekly( 11:00am - 12:15pm TU) }
			}
			
			event colloquium {
				times { weekly( 4:15pm - 5:30pm TH) }
			}
		}
		
		section other-5c-classes {
			
			event intro-to-acting {
				times { weekly(1:15pm - 3:45pm TU, TH) }
			}
			
			event squash {
				times { weekly(9:00am - 9:45am MO, WE) }
			}
		}
	}

	section finals {
	        dates {
	        	includes { 5/2/2016 - 5/6/2016 }
	        }

	        event senior-finals {
	        	times { weekly(8:00am - 5:00pm TH, FR)}
	        }
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
