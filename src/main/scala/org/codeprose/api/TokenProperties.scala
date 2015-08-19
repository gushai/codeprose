package org.codeprose.api

// OK ... 
trait SourcePosition {}

class OffsetSourcePosition(val filename: String, val offset: Int) extends SourcePosition {
   override def toString() : String = { s"""($filename,$offset)""" }
}

class OffsetSourcePositionWithTokenId(
    val filename: String, val offset: Int, val tokenId: Int) extends SourcePosition {
  override def toString() : String = { s"""($filename,$offset,$tokenId)""" }
}

class ERangePosition(val filename: String, val offset: Int, val start: Int, val end: Int){
  override def toString() : String = { s"""($filename,$offset,$start,$end)""" }
}

class ERangePositionWithTokenIds(
    filename: String, 
    offset: Int, 
    start: Int, 
    end: Int, 
    val tokenIds: List[Int]) 
    extends ERangePosition (filename, offset, start, end) {
  override def toString() : String = { s"""($filename,$offset,$start,$end,$tokenIds)""" }
}

class SymbolInfo(
  val typeId: Int,
  val fullname: String,
  val whereUsedWithinFile: List[Int]
) {
  
}


trait EntityInfo {
  def name: String 
  def members: Iterable[EntityInfo]
}

trait TypeInfo extends EntityInfo {
  def name: String
  def typeId: Int
  def declAs: String
  def fullName: String
  def typeArgs: Iterable[TypeInfo]
  def members: Iterable[EntityInfo]
  def pos: Option[SourcePosition]
  def outerTypeId: Option[Int]

  final def declaredAs = declAs
  final def args = typeArgs
}



case class TypeInformation(
    typeId: Int,
    fullname: String,
    interfaces: List[String]) {
   override def toString() : String = { s"""($typeId,$fullname,$interfaces)""" }  
}

//class TypeInfo(
//    name: String, 
//    members: Iterable[EntityInfo],
//    typeId: Int,
//    declAs: String,
//    fullName: String,
//    typeArgs: Iterable[TypeInfo],
//    pos: Option[SourcePosition],
//    outerTypeId: Option[Int]
//    ) extends EntityInfo(name,members) {
//  
//}



//class SourcePosition(val filename: String, val offset: Int){
//  override def toString() : String = {
//    s"""($filename,$offset)"""
//  }
//}
//
//class SourcePositionWithTokenId(val filename: String, val tokenId: Int){
//override def toString() : String = {
//    s"""($filename,$tokenId)"""
//  }
//}
//
//class ERangePosition(val filename: String, val offset: Int, val start: Int, val end: Int){
//  override def toString() : String = {
//    s"""($filename,$offset,$start,$end)"""
//  }
//}


class ArgumentList(){
  ???
}


class ImplicitConversionInfo(
//    val fullname: String,
//    val typeId: Int,
//    val args: String,
//    val typeArgs: String,
//    val whereUsedWithinFile_OffsetBased: SourcePosition,
//    var whereUsedWithinFile_TokenBase: SourcePositionWithTokenId
    ) {}

class ImplicitParameterInfo(){}



  
