package org.codeprose.broker

import java.io.File
import org.codeprose.consumer.WriterHtml
import org.codeprose.consumer.Consumer
import org.codeprose.provider.scalalang.EnsimeProvider
import org.codeprose.util.FileUtil
import com.typesafe.scalalogging.LazyLogging
import org.codeprose.provider.ProviderContext
import org.codeprose.provider.scalalang.EnsimeProvider
import org.codeprose.provider.scalalang.EnsimeProviderContext
import org.codeprose.consumer.WriterContextHtml
import org.codeprose.api.ProjectInfo

/**
 * Provides information to the broker.
 * Assumes ensime-server started externally.  
 * 
 * @param host            Ensime-server host.
 * @param port            Ensime-server port.
 * @param ensimeFile      Path to the project's .enimse file.
 * @param srcMainFolders  List of folder paths to process.
 *                        Default src/main/scala or src/test/scala
 * @param filesToProcess  List of files to process.
 * @param outputPath      Output path.
 * @param outputType      Output type. ("html")
 * @param verbose         Print verbose output.
 */
class BrokerContextScala(
    val host: String,
    val port: Int,
    val ensimeFile: File,
    val srcMainFolders : List[String],
    val filesToProcess: List[File],
    val outputPath: File,
    val outputType: String,
    val verbose: Boolean
    ) extends BrokerContext

/**
 * Broker for Scala.
 * 
 * Central point in codeprose scala. 
 *  - Analyzes source code with the help of an EnsimeProvider
 *  - Produces output with a WriterHtml.
 * 
 * @param bc BrokerContextScala 
 */
class BrokerScala()(implicit bc: BrokerContextScala)
    extends Broker with LazyLogging {
    
  
  private val provider = createProvider()
  private val consumer = createConsumer()
    
  
  /**
   * Creates a provider.
   * 
   * The EnsimeProvider is not initialized!
   * 
   * @return EnsimeProvider
   */
  private def createProvider() : EnsimeProvider = {
      val pc = new EnsimeProviderContext(bc.host,bc.port,bc.verbose,bc.srcMainFolders)
      new EnsimeProvider()(pc)
  }
    
  /**
   * Creates the consumer.
   * @return Consumer 
   */
  private def createConsumer() : Consumer = {
    if(bc.outputType == "html") {      
      implicit val c = new WriterContextHtml(bc.outputPath,bc.verbose)  
      new WriterHtml()     
    } else {
      throw new Exception("Unknown output type requested!")
    }
  }
  
  /**
   * Initializes consumers and provider.
   */
  def initialize() : Unit = {
    logger.info("Initializing provider and consumer ... ")
    provider.initialize()
    consumer.initialize()
  }
  
  /**
   * Analyzes the source code.
   * @return ProjectInfo 
   */
  def analyzeSourceCode() :  ProjectInfo = {
    logger.info("Analysing source code ... ")
    provider.getProjectInformation(bc.filesToProcess.toList)
  }
   
  /**
   * Generate an output.
   * @param projectInfo ProjectInfo returned by analyzeSourceCode()
   */
  def generateOutput(projectInfo: ProjectInfo): Unit = {
   consumer.generateOutput(projectInfo: ProjectInfo)
  }
    
  /**
   * Closes the provider.
   * 
   * Needs to be called before termination of BrokerScala to avoid leaks.
   */
  def close() : Unit = {
    provider.close()
    consumer.close()
  }

  
}