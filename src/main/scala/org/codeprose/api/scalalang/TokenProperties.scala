package org.codeprose.api.scalalang
import org.codeprose.api._

/*
 * Contains Scala specific classes to enrich Scala tokens.
 */

/*
 * The elements are copied with minor adjustments from org.ensime.api.outgoing.scala. 
 * See 
 * https://github.com/ensime/ensime-server/blob/master/api/src/main/scala/org/ensime/api/outgoing.scala
 *  
 */

/**
 * Source positions modelled after scala compiler type.
 * @param filename  Source filename
 * @param offset    Position within file.
 * @param start     
 * @param end 
 * @param tokenId   TokenId found at offset.
 */
case class ERangePositionWithTokenId(
    filename: String, 
    offset: Int, 
    start: Int, 
    end: Int, 
    tokenId: Int){ 
  override def toString() : String = { s"""($filename,$offset,$start,$end,$tokenId)""" }
}


/**
 * Companion objects to ERangePositionWithTokenId providing ordering.
 */
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


/**
 * Declared as.
 */
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

/**
 * SymboInfo is used to describe the properties of a token.
 *@param  name        Name.
 *@param  localName   Local name.
 *@param  declPos     Source position of declaration.
 *@param  tpe         Type information of symbol.
 *@param  isCallable  Is callable indicator.
 *@param  ownerTypeId Type id of the owner type.
 */
case class SymbolInfo(
    name: String,
    localName: String,
    declPos: Option[SourcePosition],
    tpe: TypeInfo,
    isCallable: Boolean,
    ownerTypeId: Option[Int]
) {}

