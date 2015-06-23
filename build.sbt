lazy val commonSettings = Seq(
	organization := "org.codeprose",
	version := "0.0.1",
	scalaVersion := "2.11.4"
)


libraryDependencies ++= Seq(
   	"org.scalatest"	 %% "scalatest"	 % "2.2.1"	 % "test",
	"org.scalariform" %% "scalariform" % "0.1.6",
	"org.ensime" %% "client" % "0.9.10-SNAPSHOT"
)

lazy val root = (project in file(".")).
	settings(commonSettings: _*).
	settings(
		name := "codeprose"
	)
