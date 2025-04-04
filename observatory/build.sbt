course := "capstone"
assignment := "observatory"

scalaVersion := "2.12.8"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-Xlint",
)

resolvers ++= Seq(
  "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases",
  "Artima Maven Repository" at "http://repo.artima.com/releases"
)

libraryDependencies ++= Seq(
  "com.sksamuel.scrimage" %% "scrimage-core" % "2.1.8", // for visualization
  // You don’t *have to* use Spark, but in case you want to, we have added the dependency
  "org.apache.spark" %% "spark-sql" % "2.4.3",
  // You don’t *have to* use akka-stream, but in case you want to, we have added the dependency
  "com.typesafe.akka" %% "akka-stream" % "2.5.23",
  "com.typesafe.akka" %% "akka-stream-testkit" % "2.5.23" % Test,
  // You don’t *have to* use Monix, but in case you want to, we have added the dependency
  "io.monix" %% "monix" % "2.3.3",
  // You don’t *have to* use fs2, but in case you want to, we have added the dependency
  "co.fs2" %% "fs2-io" % "1.0.5",
  "org.scalacheck" %% "scalacheck" % "1.13.5" % Test,
  "com.novocode" % "junit-interface" % "0.11" % Test,
  "com.storm-enroute" %% "scalameter" % "0.17",
  "org.scalactic" %% "scalactic" % "3.1.0",
  "org.scalatest" %% "scalatest" % "3.1.0" % Test
)
addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")
testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")

parallelExecution in Test := false // So that tests are executed for each milestone, one after the other
