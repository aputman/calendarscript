import AssemblyKeys._ 

assemblySettings

jarName in assembly := "RCal.jar"

val meta = """META.INF(.)*""".r

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList("javax", "servlet", xs @ _*)         => MergeStrategy.first
    case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
    case "application.conf" => MergeStrategy.concat
    case meta(_) => MergeStrategy.discard
    case x => old(x)
  }
}