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







case class CodeproseScalaConfig(
    ensimeFile: File = new File("."),
    inputFolder: File = new File("."),
    outputFolder: File = new File("."),
    ensimeHost: String = "127.0.0.1",
    includeTests: Boolean = false,
    verbose: Boolean = false
    )
  
  
/**
 * 
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
  
  

   
    
    
    
    
//    // Get inputs
//    if(args.length!=2 ){      
//      println(help())
//      return
//    }
//    
//    val ensimeFile = new File(args(0))    
//    val mainSrcFolder = new File(ensimeFile.getParent() +"/src/main/scala/")
//    val outputPath = if(!args(1).endsWith("/")){
//        new File(args(1)+"/")
//    } else new File(args(1))
//            
//    val host = "127.0.0.1"
//    val pathToPortFile=EnsimeServerUtil.getPathToPortFile(ensimeFile)
//    val port = if(pathToPortFile.exists()){
//      EnsimeServerUtil.readPortFromPortFile(pathToPortFile)
//    } else -1
//    val verbose = true
//    val outputType = "html"
//    
//    if(port == -1){
//      logger.error("Port file could not be determined! Shutting down.")
//      return
//    }
//    
//    
//    // create broker context 
//    val bc = getBrokerContext(host,port,ensimeFile,mainSrcFolder,outputPath,verbose,outputType)    
//    runCodeprose(bc) 
    
  }
  
  
  def getConfigParser() : OptionParser[CodeproseScalaConfig] = {
    val parser = new scopt.OptionParser[CodeproseScalaConfig]("codeprose") {
      head("codeprose", "0.314")
    
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
      
      help("help") text("prints this usage text")
       
    }
    parser
  }
  
  def runCodeprose(implicit bc: BrokerContext): Unit = {
    logger.info("Run starting codeprose broker.")
    val broker = new CodeproseBroker()
    val info = broker.analyzeSourceCode()
    broker.generateOutput(info)           
    broker.close()
    
  }
  
  def getBrokerContext(
    host: String,
    port: Int,
    ensimeFile: File,
    mainSrcFolders: List[File],
    outputPath: File,
    verbose: Boolean,
    outputType: String
  ) : BrokerContext = {

    val filesToProcess = mainSrcFolders.map(folder => FileUtil.getAllScalaFilesIncludingSubDir(folder).sorted).flatten      
    return new BrokerContext(host,port,ensimeFile,mainSrcFolders.map(e=>e.getAbsolutePath),filesToProcess,outputPath,outputType,verbose)   
  }
       
}

/**
 * Central point in the application. Requests information on project from
 * Provider and hands them off to the consumers.
 * 
 */
trait Broker {
  import org.codeprose.api.Api
  def analyzeSourceCode() : ProjectInfo  
  def generateOutput(projectInfo: ProjectInfo) : Unit    
  def close() : Unit
}


/**
 * Provides information to the broker.
 * Assumes ensime-server started externally.  
 */
class BrokerContext(
    val host: String,
    val port: Int,
    val ensimeFile: File,
    val srcMainFolders : List[String],
    val filesToProcess: List[File],
    val outputPath: File,
    val outputType: String,
    val verbose: Boolean
    ){}




class CodeproseBroker()(implicit bc: BrokerContext)
    extends Broker with LazyLogging {
    
  import org.codeprose.api.Api
  
  private val provider = initializeProvider()
  private val consumer = initializeWriter()
    
  private def initializeProvider() : EnsimeProvider = {
      val pc = new EnsimeProviderContext(bc.host,bc.port,bc.verbose,bc.srcMainFolders)
      val p = new EnsimeProvider()(pc)
      p.initialize()
      return p
  }
    
    
  def analyzeSourceCode() :  ProjectInfo = {
    import org.codeprose.api.Api
    logger.info("Analysing source code ... ")
    val projectInfo = provider.getProjectInformation(bc.filesToProcess.toList)
    projectInfo
  }
   
  def generateOutput(projectInfo: ProjectInfo): Unit = {
   consumer.generateOutput(projectInfo: ProjectInfo)
  }
    
  def close() : Unit = {
    provider.close()
  }

  private def initializeWriter() : Consumer = {
    if(bc.outputType == "html") {      
      implicit val c = new WriterContextHtml(bc.outputPath,true)  
      new WriterHtml()     
    } else {
      throw new Exception("Unknown output type requested!")
    }
    
  }
}