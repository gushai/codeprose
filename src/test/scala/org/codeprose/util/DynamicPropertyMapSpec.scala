package org.codeprose.util

import org.scalatest.FunSpec

/*
 *  Setup for key and extended keys.
 *  This simulates the DefaultLang and ScalaLang specifications in org.codeprose.api.Api
 */
trait DefaultSpec {
  /*
   * Simulates the tokenType property in DefaultLang.
   */
  trait TT {    
    val bar : String
  }
  
  import org.codeprose.util.DynamicPropertyMap.Key    
  // Keys
  val tt = new Key('tokenType) { type Value <: TT }
  val bar = new Key('bar) { type Value = Int }
  val foo = new Key('foo) { type Value = String }   
}

/*
 * By extending DefaultSpec FancySpec simulates ScalaLang. 
 */
trait FancySpec extends DefaultSpec {
  import org.codeprose.util.DynamicPropertyMap.Key
  
  class FancyTT(val bar: String) extends TT { def foo : Boolean = true }
  
  // Keys
  override val tt = new Key('fancyTT) { type Value = FancyTT }
  val fullName = new Key('fullName) { type Value = String }
}
object FancySpec extends FancySpec 

/**
 * DynamicPropertyMap
 * @param foo Some input.
 */
case class DPM(val foo: Int) extends DynamicPropertyMap

/**
 * Test class for DynamicPropertyMap.
 */
class DynamicPropertyMapSpec extends FunSpec {
 
  
  describe("A DynamicPropertyMap"){
    
    it("should add and remove elements from FancySpec:"){
      
      import FancySpec._
      
      // Add stuff
      val dpm = DPM(42)
      dpm.set(bar)(314)
      assert(dpm.size() == 1)
      dpm.set(foo)("foo")
      assert(dpm.size() == 2)
      dpm.set(fullName)("name")
      assert(dpm.size() == 3)
      
      dpm.set(tt)(new FancyTT("bar"))
      assert(dpm.size() == 4)
      
      // Check stuff
      assert(dpm.foo == 42)
      
      dpm(bar) match{
        case Some(bar)  => assert(bar == 314)
        case None => fail()
      }
      dpm.remove(bar) 
      assert(dpm.size() == 3)
      
      dpm(foo) match{
        case Some(foo)  => assert(foo == "foo")
        case None => fail()
      }
      dpm.remove(foo)
      
      dpm(fullName) match{
        case Some(fullName)  => assert(fullName == "name")
        case None => fail()
      }
      dpm.remove(fullName)
      
      dpm.get(tt) match {
        case Some(tt) => assert(tt.foo == true && tt.bar == "bar")
        case None => fail()
      }
      dpm.remove(tt)
      
      assert(dpm.size() == 0)
    }
    
    it("should be able to handle requests for key that are not in the DPM") {
      import FancySpec._
      val dpm = DPM(42)
      
      assert(dpm(tt) == None)
      assert(dpm(foo) == None)
      assert(dpm(fullName) == None)
    }
    
    it("should be able to update existing keys"){
       import FancySpec._
       val dpm = DPM(1)
       
       dpm.set(foo)("asdf")
       dpm.set(foo)("changed")
       dpm(foo) match {
         case Some(s) => assert(s == "changed")
         case None => fail()
       }
      
    }
    
    it("should print a information summary of the comonents"){
      import FancySpec._
      val dpm = DPM(42)
      dpm.set(bar)(314)
      dpm.set(foo)("foo")
      dpm.set(fullName)("name")           
      println(dpm.toString())
    }
  }
}
