package calendarscript

import scala.tools.nsc.EvalLoop
import calendarscript._
import calendarscript.parser._
import calendarscript.ir._
import java.io.File
import net.fortuna.ical4j.model._
//import scalafx.application.JFXApp
/**
 * @author aputman
 */

object Engine extends interpreter {
//  override def prompt = "> "
//
//  loop { line ⇒
//    PiconotParser(line) match {
//      case PiconotParser.Success(t, _) ⇒ println(eval(t))
//      case e: PiconotParser.NoSuccess ⇒ println(e)
//    }
//  }
  def main(args: Array[String]) = {
    val lines = scala.io.Source.fromFile("resources" + File.separator + s"test.cal").mkString
    CalendarParser(lines) match {
      case CalendarParser.Success(t, _) ⇒ {
        println(t)
        var result = evalCal(t)
        println(result)
      }
      case e: CalendarParser.NoSuccess  ⇒ println(e)
    }
  }
}


