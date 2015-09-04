package org.codeprose.api.scalalang

import org.codeprose.api._

case class ERangePositionWithTokenId(
    filename: String, 
    offset: Int, 
    start: Int, 
    end: Int, 
    tokenId: Int){ 
  override def toString() : String = { s"""($filename,$offset,$start,$end,$tokenId)""" }
}

object ERangePositionWithTokenId {
  implicit val ord = new Ordering[ERangePositionWithTokenId] {
    def compare(a: ERangePositionWithTokenId, b: ERangePositionWithTokenId): Int = {
      if(a.filename == b.filename){
        a.tokenId compare b.tokenId  
      } else {
        a.filename compare b.filename
      }
      
    }
  }
}  


sealed abstract class DeclaredAs(val symbol: scala.Symbol)

object DeclaredAs {
  case object Method extends DeclaredAs('method)
  case object Trait extends DeclaredAs('trait)
  case object Interface extends DeclaredAs('interface)
  case object Object extends DeclaredAs('object)
  case object Class extends DeclaredAs('class)
  case object Field extends DeclaredAs('field)
  case object Nil extends DeclaredAs('nil)

  def allDeclarations = Seq(Method, Trait, Interface, Object, Class, Field, Nil)
}


case class SymbolInfo(
    name: String,
    localName: String,
    declPos: Option[SourcePosition],
    tpe: TypeInfo,
    isCallable: Boolean,
    ownerTypeId: Option[Int]
) {}

