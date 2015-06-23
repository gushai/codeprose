package org.codeprose.broker

import java.io.File
import org.codeprose.util.EnsimeServerUtil
import scala.util.{Success, Failure}
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import org.codeprose.util.FileUtil
import org.codeprose.provider.Tokenizer
import org.codeprose.provider.EnsimeProvider
import scalariform.lexer.Token
import org.codeprose.consumer.WriterHtml
import org.codeprose.consumer.Consumer


object Broker {
  
  def main(args: Array[String]): Unit = {
    
    if(args.length!=2){      
      println(help())
    }else{
      val ensimeFile = new File(args(0))    
      val mainSrcFolder = new File(ensimeFile.getParent() +"/src/main/scala/")
      val outputPath = if(!args(1).endsWith("/")){
        new File(args(1)+"/")
      } else new File(args(1))
      
      codeprose(ensimeFile,mainSrcFolder,outputPath)
    }
  }

  
  
  def codeprose(ensimeFile: File, mainSrcFolder: File, outputPath: File) : Unit = {
        
    // Start broker
    val broker = new Broker(ensimeFile,mainSrcFolder,outputPath)
    
//    val b=try {
//      
//     } catch {
//      case e : Throwable => println("[Broker]:\t" + "Could not determine port. " + e.getMessage )       
//    } finally {
//      //broker.serverProcess.destroy()
//      broker.close()
//    }
//        
    val info = broker.analyzeSourceCode()
    broker.generateOutput(info)
        
    broker.close()
    
  }
  
  def help() : String = {
    s"""Inputs required:\n\t(1) Path to .ensime file \n\t(2) Output path."""  
  }
  
}


class Broker(ensimeFile: File, mainSrcFolder: File, outputPath: File){
  
  val host = "127.0.0.1"
  val port =  setPort()
  
  val filesToProcess = getListOfSourceFiles()
  //val serverProcess = EnsimeServerUtil.startEnsimeServer(ensimeFile)
  
  private val provider = initializeProvider()
  private val consumer = initializeWriter("html")
  
  
  private def setPort() : Int = {       
    val portF = EnsimeServerUtil.getPort(ensimeFile) 
    portF onComplete {
      case Success(port) => {return port}
      case Failure(t) => {
        return -1        
        }      
    }            
    Await.result(portF,  3.seconds)    
  }
  
  private def initializeProvider() : EnsimeProvider = {
    if(port != -1){
      return new EnsimeProvider(host,port)
    } else return null
  }
    
  // Get list of files in the project
  private def getListOfSourceFiles() : Array[File] = {
    FileUtil.getAllScalaFilesIncludingSubDir(mainSrcFolder)
  }
      
  // For each file get tokens and enrich them
  def analyzeSourceCode() : scala.collection.mutable.ArrayBuffer[
    (java.io.File, scala.collection.mutable.Map[Int,(Token,List[(String, String)] )])] 
  = {
    
    val out = scala.collection.mutable.ArrayBuffer[(java.io.File, scala.collection.mutable.Map[Int,(Token,List[(String, String)] )])]()

    for(f <- filesToProcess){
      
      // Get enriched tokens
      println("[Broker]:\t" + f.toString())
      val tokens = Tokenizer.tokenize(f)
      //tokens.foreach(t => if (t.offset < 100) println(t.toString() ))
      val enrichedTokens = provider.enrichTokens(f, tokens)        
     /*
      enrichedTokens.foreach( 
          p => println("[BROKER]:\t" + "Offset: "+p._1.toString() + "\tToken: " + p._2._1 + "\tProp:" + p._2._2.toString())         
      )*/                                     
     out += ((f,enrichedTokens))
    } 
    out
  }
  
  private def analyzeSourceCodeOverview() : Unit = {
    ???
  }
  
  // Generate output
  private def generateOutput(
      info: 
      scala.collection.mutable.ArrayBuffer[(java.io.File, scala.collection.mutable.Map[Int,(Token,List[(String, String)] )])]) 
  : Unit = {
   consumer.generateOutput(info)
  }
    
  def close() : Unit = {
    provider.close()
  }

  def initializeWriter(typ: String) : Consumer = {
    if(typ.equals("html")) {
     return new WriterHtml(outputPath) 
    } else null
  }
}