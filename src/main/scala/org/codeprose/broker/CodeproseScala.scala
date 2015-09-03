package org.codeprose.broker

import java.io.File
import org.codeprose.util.EnsimeServerUtil
import scala.util.{Success, Failure}
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import org.codeprose.api.Token
import org.codeprose.consumer.WriterHtml
import org.codeprose.consumer.Consumer
import org.codeprose.provider.Tokenizer
import org.codeprose.provider.EnsimeProvider
import org.codeprose.util.FileUtil
import com.typesafe.scalalogging.LazyLogging
import org.codeprose.provider.ProviderContext
import org.codeprose.provider.EnsimeProvider
import org.codeprose.provider.EnsimeProviderContext
import org.codeprose.consumer.WriterContextHtml
import org.codeprose.api.ProjectInfo
import scopt.OptionParser



/**
 * Codeprose scala application. 
 */
object Codeprose extends LazyLogging {
  
  def main(args: Array[String]): Unit = {
  
    val configParser = getConfigParser()
  
    configParser.parse(args, CodeproseScalaConfig()) match {
      case Some(config) =>
        val ensimeFile = config.ensimeFile    
        val outputPath = config.outputFolder
        val mainSrcFolders = if(config.includeTests){
          List(new File(config.inputFolder.getPath + "/src/main/scala/"),
               new File(config.inputFolder.getPath + "/src/test/scala/"))
        } else {
           List(new File(config.inputFolder.getPath + "/src/main/scala/"))
        }
        
        val host = config.ensimeHost
        val pathToPortFile=EnsimeServerUtil.getPathToPortFile(ensimeFile)
        val ensimePort = if(pathToPortFile.exists()){
          EnsimeServerUtil.readPortFromPortFile(pathToPortFile)
        } else -1
        
        val verbose = config.verbose
        val outputType = "html"
        
        if(ensimePort == -1){
          logger.error("Ensime port file could not be determined! Shutting down.")
          return
        }
        
        
      // create broker context 
      val bc = getBrokerContext(host,ensimePort,ensimeFile,mainSrcFolders,outputPath,verbose,outputType)
      runCodeprose(bc) 
        
      case None => {}
    }
  }
  
  /**
   * Parses the args input of the main and constructs a input config. 
   * @return OptionParser[CodeproseScalaConfig] input options for codeprose scala.
   */
  def getConfigParser() : OptionParser[CodeproseScalaConfig] = {
    val parser = new scopt.OptionParser[CodeproseScalaConfig]("codeprose") {
      head("codeprose", "0.1")
    
      opt[File]('e', "ensimeFile") required() valueName("<file>") action { (x, c) =>
        c.copy(ensimeFile = x) } text("ensimeFile: Path to the ensime file of the project.")
      
      opt[File]('o', "outputFolder") required() valueName("<file>") action { (x, c) =>
        c.copy(outputFolder = x) } text("outputFolder: Path to the output folder.")
        
      opt[File]('i', "inputFolder") required() valueName("<file>") action { (x, c) =>
        c.copy(inputFolder = x) } text("inputFolder: Path to the main folder of the project. \n\tAssumes sbt project structure with ./src/main/scala/ and ./src/test/scala/.\n\tDefault all scala files in ./src/main/scala/ and its sub directories are processed.")
  
      opt[String]("ensimeHost") valueName("<ip address>") action { (x, c) =>
        c.copy(ensimeHost = x) } text("ensimeHost: IP Address of the ensime host (default: 127.0.0.1).")
      
      opt[Unit]("includeTests") action { (_, c) =>
        c.copy(includeTests = true) } text("includeTests is a flag to include the files in inputFolder/src/test/scala/")  
  
      opt[Unit]("verbose") action { (_, c) =>
        c.copy(verbose = true) } text("verbose is a flag")  
        
      note("\nExample:\n")  
      note(s"""\t codeprose -e "/pathTo/.ensime" -o "pathTo/output/" -i "pathTo/input/ --includeTests""")
      
      note("\nNotes:\n")
      note(s"""\t- codeprose processes only .scala files. All others are ignored.""")
      help("help") text("prints this usage text")
       
    }
    parser
  }
  
  
  /**
   * Runs the scala broker.
   * 
   * @param bc BrokerContextScala with information needed for BrokerScala.
   */
  def runCodeprose(implicit bc: BrokerContextScala): Unit = {
    logger.info("Run starting codeprose broker.")
    val broker = new BrokerScala()
    broker.initialize()
    val info = broker.analyzeSourceCode()
    broker.generateOutput(info)           
    broker.close()
  }
  
  /**
   * Returns a context for the Scala Broker.
   * @return BrokerContextScala
   */
  def getBrokerContext(
    host: String,
    port: Int,
    ensimeFile: File,
    mainSrcFolders: List[File],
    outputPath: File,
    verbose: Boolean,
    outputType: String
  ) : BrokerContextScala = {

    val filesToProcess = mainSrcFolders.map(folder => FileUtil.getAllScalaFilesIncludingSubDir(folder).sorted).flatten      
    return new BrokerContextScala(host,port,ensimeFile,mainSrcFolders.map(e=>e.getAbsolutePath),filesToProcess,outputPath,outputType,verbose)   
  }
       
}

/**
 * Input config for the main args.
 * Used with scopt.
 */
case class CodeproseScalaConfig(
    ensimeFile: File = new File("."),
    inputFolder: File = new File("."),
    outputFolder: File = new File("."),
    ensimeHost: String = "127.0.0.1",
    includeTests: Boolean = false,
    verbose: Boolean = false)



