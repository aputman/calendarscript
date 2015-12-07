import AssemblyKeys._ 

assemblySettings

jarName in assembly := "RCal.jar"

mainClass in assembly := Some("calendarscript.Engine")
