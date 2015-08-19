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


object CodeproseBroker extends LazyLogging {
  
  def main(args: Array[String]): Unit = {
    
    // Get inputs
    if(args.length!=2 ){      
      println(help())
      return
    }
    
    val ensimeFile = new File(args(0))    
    val mainSrcFolder = new File(ensimeFile.getParent() +"/src/main/scala/")
    val outputPath = if(!args(1).endsWith("/")){
        new File(args(1)+"/")
    } else new File(args(1))
            
    val host = "127.0.0.1"
    val pathToPortFile=EnsimeServerUtil.getPathToPortFile(ensimeFile)
    val port = if(pathToPortFile.exists()){
      EnsimeServerUtil.readPortFromPortFile(pathToPortFile)
    } else -1
    val verbose = true
    val outputType = "html"
    
    if(port == -1){
      logger.error("Port file could not be determined! Shutting down.")
      return
    }
    
    
    // create broker context 
    val bc = getBrokerContext(host,port,ensimeFile,mainSrcFolder,outputPath,verbose,outputType)    
    runCodeprose(bc) 
    
  }
  
  def runCodeprose(implicit bc: BrokerContext): Unit = {
    logger.info("Run starting codeprose broker.")
    val broker = new CodeproseBroker()
    val info = broker.analyzeSourceCode()
    broker.generateOutput(info)           
    broker.close()
    
  }
  
  def help() : String = {
    s"""Inputs required:\n\t(1) Path to .ensime file \n\t(2) Output path."""  
  }

  def getBrokerContext(
    host: String,
    port: Int,
    ensimeFile: File,
    mainSrcFolder: File,
    outputPath: File,
    verbose: Boolean,
    outputType: String
  ) : BrokerContext = {

    // TODO: Extend to include several folders
    val filesToProcess = FileUtil.getAllScalaFilesIncludingSubDir(mainSrcFolder)        
    return new BrokerContext(host,port,ensimeFile,List(mainSrcFolder.getAbsolutePath),filesToProcess,outputPath,outputType,verbose)   
  }
       
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
    val filesToProcess: Array[File],
    val outputPath: File,
    val outputType: String,
    val verbose: Boolean
    ){}



trait Broker {
  import org.codeprose.api.Api
  def analyzeSourceCode() : ProjectInfo  
  def generateOutput(projectInfo: ProjectInfo) : Unit    
  def close() : Unit
}

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
  
  private def analyzeSourceCodeOverview() : Unit = {
    ???
  }
  
  def generateOutput(projectInfo: ProjectInfo): Unit = {
   consumer.generateOutput(projectInfo)
  }
    
  def close() : Unit = {
    provider.close()
  }

  private def initializeWriter() : Consumer = {
    if(bc.outputType == "html") {
      
      val c = new WriterContextHtml(true)
      
      new WriterHtml(bc.outputPath)(c)     
    } else {
      throw new Exception("Unknown OutputWriter requested!")
    }
    
  }
}