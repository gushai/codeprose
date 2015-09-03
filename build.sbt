lazy val commonSettings = Seq(
	organization := "org.codeprose",
	version := "0.0.1",
	scalaVersion := "2.11.4"
)


libraryDependencies ++= Seq(
   	"org.scalatest"	 %% "scalatest"	 % "2.2.1"	 % "test",
	"org.scalariform" %% "scalariform" % "0.1.6",
	"org.scalatra.scalate" %% "scalamd" % "1.6.1",
	"com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
	"org.ensime" %% "client" % "0.9.10-SNAPSHOT",
	"com.github.scopt" %% "scopt" % "3.3.0",
	"io.spray" %%  "spray-json" % "1.3.2"
)

lazy val root = (project in file(".")).
	settings(commonSettings: _*).
	settings(
		name := "codeprose"
	)
