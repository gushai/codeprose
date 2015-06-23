package org.codeprose.provider

import org.ensime.client.Client
import scalariform.lexer.Token
import java.io.File
import scalariform.lexer.Tokens
import org.ensime.model.OffsetRange
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Failure}

trait TokenEnricher {
   def initialize() : Unit 
   def close() : Unit 
   //def enrichTokens(file : File, tokens: List[Token]) : scala.collection.mutable.Map[Token,List[(String,String)]] 
   def enrichTokens(file : File, tokens: List[Token]) : scala.collection.mutable.Map[Int,(Token,List[(String,String)])]
}


class EnsimeProvider(host: String, port: Int) extends TokenEnricher {

  private val ensimeClient = new Client(host,port)
  ensimeClient.initialize()
  var isInitialized = false
  
  def shutdownServer() : Unit = {
    ensimeClient.shutdownServer()
  }
  
  def log(s: String) : Unit = {
    println("[EnsimeProvider]:\t"+s)
  }
  
  def initialize(): Unit = {
    
    ensimeClient.initialize()
    log("Initialized Ensime client.")
    /// TODO: 
    // Get connection info and initialize project
    // Requires a more sophisticated verion of the ensimeclient to return information on messages that have no return point
    
    val connectionInto = ensimeClient.connectionInfo()
    log("Connection Info: " + connectionInto.toString())
    
    // send
    //Init project message
    isInitialized = true
  }
  
  def close() : Unit = {
    ensimeClient.close()
  }

  def enrichTokens(file: File, tokens: List[Token]): scala.collection.mutable.Map[Int,(Token, List[(String,String)])] = {
   
    val information = scala.collection.mutable.Map[Int,(Token,List[(String,String)])]()
        
    for (token <- tokens){
     
      token.tokenType match {               
        case Tokens.VARID => {
         
    	  
    	  val typeInfo = ensimeClient.typeAtPoint(file, OffsetRange(token.offset))
        
    	  typeInfo onSuccess({
    	  case Some(t) => {
          if(!t.pos.isDefined)
    		  {
            information += (token.offset ->(token,List[(String,String)](
                ("NAME",t.fullName),
                ("TYPEID",t.typeId.toString),
                ("DECLAS",t.declAs.toString))))
          }
          else{
              information += (token.offset -> (token,List[(String,String)](
                  ("NAME",t.fullName),
                  ("TYPEID",t.typeId.toString),
                  ("DECLAS",t.declAs.toString),
                  ("POS-PATH",t.pos.get.asInstanceOf[org.ensime.model.OffsetSourcePosition].file.getAbsolutePath),
                  ("POS-OFFSET",t.pos.get.asInstanceOf[org.ensime.model.OffsetSourcePosition].offset.toString)
                  )))
          }
    				  

    	  }
    	  case None => {
    		  information += (token.offset -> (token,List[(String,String)]()))
    	  }
    	  })

    	  typeInfo onFailure({          
    	  case _ => {
    		  information += (token.offset -> (token,List[(String,String)]()))
    	  }
    	  })    
      } 
      case _ => {
          information += (token.offset -> (token -> List[(String,String)]()))    
      }
      }     
    }
    
    println("[EnsimeProvider]:\t" +"Awaiting results ... ")
    Thread.sleep(4000)
    
    return information
  }
  
  def getConnectionInfo() : Unit = {
    val f = ensimeClient.connectionInfo()       
    f onComplete {
      case Success(c) => {println(c.toString())}
      case Failure(c) => {println("boahhhh connection info failed!")}      
    }          
  }
  
  
  
}