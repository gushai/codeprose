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
    
    if(port == -1){
      logger.error("Port file could not be determined! Shutting down.")
      return
    }
    
    
    // create broker context 
    val bc = getBrokerContext(host,port,ensimeFile,mainSrcFolder,outputPath)    
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
    outputPath: File    
  ) : BrokerContext = {

    // TODO: Extend to include several folders
    val filesToProcess = FileUtil.getAllScalaFilesIncludingSubDir(mainSrcFolder)        
    return new BrokerContext(host,port,ensimeFile,filesToProcess,outputPath)   
  }
       
}

/*
 * Provides information to the broker.
 * Assumes ensime-server started externally.  
 */
class BrokerContext(
    val host: String,
    val port: Int,
    val ensimeFile: File,
    val filesToProcess: Array[File],
    val outputPath: File
    ){}


// TODO: Extension to Meta information needed.
trait Broker {
  import org.codeprose.api.Api
  def analyzeSourceCode() : Api.TokenInfoContainer  
  def generateOutput(info: Api.TokenInfoContainer) : Unit    
  def close() : Unit
}

class CodeproseBroker()(implicit bc: BrokerContext)
    extends Broker with LazyLogging {
    
  import org.codeprose.api.Api
  
  private val provider = initializeProvider()
  private val consumer = initializeWriter("html")
    
  private def initializeProvider() : EnsimeProvider = {
      return new EnsimeProvider(bc.host,bc.port)
  }
    
    
  def analyzeSourceCode() : Api.TokenInfoContainer = {
    // For each file get tokens and enrich them
    import org.codeprose.api.Api
    logger.info("Analysing source code ... ")
    val out = new Api.TokenInfoContainer()

    for(f <- bc.filesToProcess){
      logger.info(f.toString())
      val tokens = provider.getEnrichedTokens(f)                                    
     out += ((f,tokens))
    } 
    out
  }
  
  private def analyzeSourceCodeOverview() : Unit = {
    ???
  }
  
  def generateOutput(info: Api.TokenInfoContainer): Unit = {    
   consumer.generateOutput(info)
  }
    
  def close() : Unit = {
    provider.close()
  }

  private def initializeWriter(typ: String) : Consumer = {
    if(typ.equals("html")) {
     return new WriterHtml(bc.outputPath) 
    } else null
  }
}