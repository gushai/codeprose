package org.codeprose.provider

import org.scalatest.FunSpec

class EnsimeProviderSpec extends FunSpec {

  describe("An EnsimeProvider"){
    
    it("should provide a connection test"){
      val c = new EnsimeProviderContext("127.0.0.1",41959,true,List[String]())
      val ep = new EnsimeProvider()(c)
      val serverReady = ep.testConnection()
      
      assert(serverReady == true)
      
    }
  }
}