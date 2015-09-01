package org.codeprose.api

// OK ... 
trait SourcePosition {}

case class OffsetSourcePosition(val filename: String, val offset: Int) extends SourcePosition {
   override def toString() : String = { s"""($filename,$offset)""" }
}

case class OffsetSourcePositionWithTokenId(
    val filename: String, val offset: Int, val tokenId: Int) extends SourcePosition {
  override def toString() : String = { s"""($filename,$offset,$tokenId)""" }
}

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
  
case class SourcePositionLinkWithCodeSample(
    srcFilename: String,
    link: String,
    tokenId: Int,
    sourceSample: List[String]){
  override def toString() : String = { val sample = sourceSample.mkString("") 
    s"""($srcFilename,$link,$tokenId,$sample)""" }
}





// Copy of ensime information types.



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

trait EntityInfo {
  def name: String 
  def members: Iterable[EntityInfo]
  def _infoType: String
}

case class PackageInfo(
    name: String,
    fullName: String,
    members: Seq[EntityInfo]
) extends EntityInfo {def _infoType = "PackageInfo"}

case class NamedTypeMemberInfo(
    name: String,
    tpe: TypeInfo,
    pos: Option[SourcePosition],
    signatureString: Option[String],
    declAs: DeclaredAs
) extends EntityInfo {
  override def members = List.empty
  def _infoType = "NamedTypeMemberInfo"
}


sealed trait TypeInfo extends EntityInfo {
  def name: String
  def typeId: Int
  def declAs: DeclaredAs
  def fullName: String
  def typeArgs: Iterable[TypeInfo]
  def members: Iterable[EntityInfo]
  def pos: Option[SourcePosition]
  def outerTypeId: Option[Int]
 // final def declaredAs = declAs
 // final def args = typeArgs
}


case class BasicTypeInfo(
  name: String,
  typeId: Int,
  declAs: DeclaredAs,
  fullName: String,
  typeArgs: Iterable[TypeInfo],
  members: Iterable[EntityInfo],
  pos: Option[SourcePosition],
  outerTypeId: Option[Int]
) extends TypeInfo {
  def _infoType = "BasicTypeInfo"
}

case class ArrowTypeInfo(
    name: String,
    typeId: Int,
    resultType: TypeInfo,
    paramSections: Iterable[ParamSectionInfo]
) extends TypeInfo {
  def declAs = DeclaredAs.Nil
  def fullName = name
  def typeArgs = List.empty
  def members = List.empty
  def pos = None
  def outerTypeId = None
  def _infoType = "ArrowTypeInfo"
}

case class ParamSectionInfo(
  params: Iterable[(String, TypeInfo)],
  isImplicit: Boolean
)

case class InterfaceInfo(
    tpe: TypeInfo,
    viaView: Option[String]
)


case class TypeInspectInfo(
    tpe: TypeInfo,
    companionId: Option[Int],
    interfaces: Iterable[InterfaceInfo]
){}


case class SymbolInfo(
    name: String,
    localName: String,
    declPos: Option[SourcePosition],
    tpe: TypeInfo,
    isCallable: Boolean,
    ownerTypeId: Option[Int]
) {}

trait ImplicitInfo

case class ImplicitConversionInfo(
  start: Int,
  end: Int,
  fun: SymbolInfo
) extends ImplicitInfo {
  def _infoType = "ImplicitConversionInfo"
}

case class ImplicitParamInfo(
  start: Int,
  end: Int,
  fun: SymbolInfo,
  params: List[SymbolInfo],
  funIsImplicit: Boolean
) extends ImplicitInfo {
  def _infoType = "ImplicitParamInfo"
}


// 

case class TypeInformation(
    typeId: Int,
    fullname: String,
    interfaces: List[String]) {
   override def toString() : String = { s"""($typeId,$fullname,$interfaces)""" }  
}





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


//class ArgumentList(){
//  ???
//}


//class ImplicitConversionInfo(
//    val fullname: String,
//    val typeId: Int,
//    val args: String,
//    val typeArgs: String,
//    val whereUsedWithinFile_OffsetBased: SourcePosition,
//    var whereUsedWithinFile_TokenBase: SourcePositionWithTokenId
//    ) {}

//class ImplicitParameterInfo(){}



  
