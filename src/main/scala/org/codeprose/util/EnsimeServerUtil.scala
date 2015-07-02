package org.codeprose.util

import scala.concurrent._
import java.io.File
import scala.io.Source
import scala.sys.process._
import ExecutionContext.Implicits.global
import com.typesafe.scalalogging.LazyLogging

object EnsimeServerUtil extends LazyLogging {

	def startEnsimeServer(ensimeFile: File) : Process ={

		// check if file exists
    if(!ensimeFile.exists){
      throw new Exception("The Ensime file does not exsits. Can not start server.")
    }
    
      val startCmd = new EnsimeServerStartCommand(ensimeFile)
      val serverProcess = Process(startCmd.getStartCmd()).run()
      serverProcess
	}

	def getPort(ensimeFile: File) : Future[Int] = {
			val numOfTries = 0
			val maxNumOfTrials = 3
			val timeToWaitMs = 1000
      val pathToEnsimePort = getPathToPortFile(ensimeFile: File)
      println(pathToEnsimePort) 
      val port : Future[Int] = Future {

				while(!pathToEnsimePort.exists() && numOfTries<maxNumOfTrials){
          logger.info("waiting for port file (" + (maxNumOfTrials-numOfTries)*timeToWaitMs + " ms left)")
					Thread.sleep(timeToWaitMs)          
				}
				if (pathToEnsimePort.exists()){
					val lines = Source.fromFile(pathToEnsimePort).getLines().mkString
							lines.toInt
				} else { 
					-1
				}
			}
			port
	}
  
  private def getPathToPortFile(ensimeFile: File) : File = {
    //val pathSep = System.getProperty("path.separator");    
    val pathSep = "/";
    var file = new File(ensimeFile.getParentFile().getAbsolutePath() + s"""/.ensime_cache/port""")
    file
  }

}

private class EnsimeServerStartCommand(ensimeFilePath: File) {

	val javaPath = """/usr/lib/jvm/java-7-openjdk-amd64/bin/java""" 
			val javaClassPath = """-classpath""" + " " + """/usr/lib/jvm/java-7-openjdk-amd64/lib/tools.jar:/home/gus/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.4.jar:/home/gus/.ivy2/local/org.ensime/ensime_2.11/0.9.10-SNAPSHOT/jars/ensime_2.11.jar:/home/gus/.ivy2/local/org.ensime/server_2.11/0.9.10-SNAPSHOT/jars/server_2.11.jar:/home/gus/.ivy2/cache/org.scala-lang/scala-compiler/jars/scala-compiler-2.11.4.jar:/home/gus/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-2.11.4.jar:/home/gus/.ivy2/cache/org.scala-lang.modules/scala-xml_2.11/bundles/scala-xml_2.11-1.0.2.jar:/home/gus/.ivy2/cache/org.scala-lang.modules/scala-parser-combinators_2.11/bundles/scala-parser-combinators_2.11-1.0.2.jar:/home/gus/.ivy2/local/org.ensime/api_2.11/0.9.10-SNAPSHOT/jars/api_2.11.jar:/home/gus/.ivy2/cache/com.github.stacycurl/pimpathon-core_2.11/jars/pimpathon-core_2.11-1.4.0.jar:/home/gus/.ivy2/cache/com.danieltrinh/scalariform_2.11/jars/scalariform_2.11-0.1.5.jar:/home/gus/.ivy2/local/org.ensime/swank_2.11/0.9.10-SNAPSHOT/jars/swank_2.11.jar:/home/gus/.ivy2/local/org.ensime/sexpress_2.11/0.9.10-SNAPSHOT/jars/sexpress_2.11.jar:/home/gus/.ivy2/cache/com.chuusai/shapeless_2.11/bundles/shapeless_2.11-2.0.0.jar:/home/gus/.ivy2/cache/org.parboiled/parboiled-scala_2.11/jars/parboiled-scala_2.11-1.1.7.jar:/home/gus/.ivy2/cache/org.parboiled/parboiled-core/jars/parboiled-core-1.1.7.jar:/home/gus/.ivy2/cache/com.typesafe.akka/akka-slf4j_2.11/jars/akka-slf4j_2.11-2.3.9.jar:/home/gus/.ivy2/cache/com.typesafe.akka/akka-actor_2.11/jars/akka-actor_2.11-2.3.9.jar:/home/gus/.ivy2/cache/com.typesafe/config/bundles/config-1.2.1.jar:/home/gus/.ivy2/cache/ch.qos.logback/logback-classic/jars/logback-classic-1.1.3.jar:/home/gus/.ivy2/cache/ch.qos.logback/logback-core/jars/logback-core-1.1.3.jar:/home/gus/.ivy2/cache/org.slf4j/jul-to-slf4j/jars/jul-to-slf4j-1.7.12.jar:/home/gus/.ivy2/cache/org.slf4j/slf4j-api/jars/slf4j-api-1.7.12.jar:/home/gus/.ivy2/cache/org.slf4j/jcl-over-slf4j/jars/jcl-over-slf4j-1.7.12.jar:/home/gus/.ivy2/cache/com.h2database/h2/jars/h2-1.4.182.jar:/home/gus/.ivy2/cache/com.typesafe.slick/slick_2.11/bundles/slick_2.11-2.1.0.jar:/home/gus/.ivy2/cache/com.jolbox/bonecp/bundles/bonecp-0.8.0.RELEASE.jar:/home/gus/.ivy2/cache/com.google.guava/guava/bundles/guava-15.0.jar:/home/gus/.ivy2/cache/org.apache.commons/commons-vfs2/jars/commons-vfs2-2.0.jar:/home/gus/.ivy2/cache/org.apache.lucene/lucene-core/jars/lucene-core-4.7.2.jar:/home/gus/.ivy2/cache/org.apache.lucene/lucene-analyzers-common/jars/lucene-analyzers-common-4.7.2.jar:/home/gus/.ivy2/cache/org.ow2.asm/asm-commons/jars/asm-commons-5.0.3.jar:/home/gus/.ivy2/cache/org.ow2.asm/asm-tree/jars/asm-tree-5.0.3.jar:/home/gus/.ivy2/cache/org.ow2.asm/asm/jars/asm-5.0.3.jar:/home/gus/.ivy2/cache/org.ow2.asm/asm-util/jars/asm-util-5.0.3.jar:/home/gus/.ivy2/cache/org.scala-lang/scalap/jars/scalap-2.11.6.jar:/home/gus/.ivy2/cache/org.scala-refactoring/org.scala-refactoring.library_2.11/jars/org.scala-refactoring.library_2.11-0.6.2.jar:/home/gus/.ivy2/cache/commons-lang/commons-lang/jars/commons-lang-2.6.jar:/home/gus/.ivy2/cache/io.spray/spray-can_2.11/bundles/spray-can_2.11-1.3.3.jar:/home/gus/.ivy2/cache/io.spray/spray-io_2.11/bundles/spray-io_2.11-1.3.3.jar:/home/gus/.ivy2/cache/io.spray/spray-util_2.11/bundles/spray-util_2.11-1.3.3.jar:/home/gus/.ivy2/cache/io.spray/spray-http_2.11/bundles/spray-http_2.11-1.3.3.jar"""
					val javaJVMInstructions = """-Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M"""
					val javaEnsimeFilePath = """-Densime.config=""" + ensimeFilePath.getAbsolutePath
					val javaEnsimeServerName = """org.ensime.server.Server"""

					val delimiter = " "

					def getStartCmd() : String = {    
							List(javaPath,javaClassPath,javaJVMInstructions,javaEnsimeFilePath,javaEnsimeServerName).mkString(delimiter)    
					}

}
