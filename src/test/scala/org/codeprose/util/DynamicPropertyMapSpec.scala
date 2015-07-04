package org.codeprose.util

import org.scalatest.FunSpec

  
// TODO: Replace with mocks?

// Setup for key and extended keys
trait DefaultSpec {
  trait TT {    
    val bar : String
  }

  import org.codeprose.util.DynamicPropertyMap.Key    

  // Keys
  val tt = new Key('tokenType) { type Value <: TT }
  val bar = new Key('bar) { type Value = Int }
  val foo = new Key('foo) { type Value = String }   
}

trait FancySpec extends DefaultSpec {
  import org.codeprose.util.DynamicPropertyMap.Key
  
  class FancyTT(val bar: String) extends TT { def foo : Boolean = true }
  
  // Keys
  override val tt = new Key('fancyTT) { type Value = FancyTT }
  val fullName = new Key('fullName) { type Value = String }
  
  // Relic of version that does not work
  // See in test. A String is not accepted in the DynamicPropertyMap.set(fullName)("foo")
  // val fullName = Key[String]('fullName)
}
object FancySpec extends FancySpec 


case class DPM(val foo: Int) extends DynamicPropertyMap

class DynamicPropertyMapSpec extends FunSpec {
 
  
  describe("A DynamicPropertyMap"){
    
    it("should add and remove elements ... "){
      
      import FancySpec._
      
      // Add stuff
      val dpm = DPM(42)
      dpm.set(bar)(314)
      assert(dpm.size() == 1)
      dpm.set(foo)("foo")
      assert(dpm.size() == 2)
      dpm.set(fullName)("name")
      assert(dpm.size() == 3)
      // Fails "name" is not of the correct type!
      // dpm.set(fullName)("name")  
      
      dpm.set(tt)(new FancyTT("bar"))
      assert(dpm.size() == 4)
      
      //println(dpm)
      
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
    
  }
  
  
}
