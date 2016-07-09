name := "ai-fun"

version := "1.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-target:jvm-1.8", "-deprecation", "-feature", "-unchecked")

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

mergeStrategy in assembly <<= (mergeStrategy in assembly) {
  (old) => {
    case s if s.endsWith("properties") => MergeStrategy.filterDistinctLines
    case s if s.endsWith("pom.xml") => MergeStrategy.last
    case s if s.endsWith(".class") => MergeStrategy.last
    case s if s.endsWith("libjansi.jnilib") => MergeStrategy.last
    case s if s.endsWith("jansi.dll") => MergeStrategy.rename
    case s if s.endsWith("libjansi.so") => MergeStrategy.rename
    case s if s.endsWith(".dtd") => MergeStrategy.rename
    case s if s.endsWith(".xsd") => MergeStrategy.rename
//    case PathList("META-INF", "services", "org.apache.hadoop.fs.FileSystem") => MergeStrategy.filterDistinctLines
    case s => old(s)
  }
}