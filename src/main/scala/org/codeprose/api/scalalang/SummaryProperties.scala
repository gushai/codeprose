package org.codeprose.api.scalalang

import org.codeprose.api.SourcePosition


/*
 * Contains Scala specific classes to enrich a Scala project.
 */

/*
 * The elements are copied with minor adjustments from org.ensime.api.outgoing.scala. 
 * See 
 * https://github.com/ensime/ensime-server/blob/master/api/src/main/scala/org/ensime/api/outgoing.scala
 *  
 */


/**
 * TypeName helps to identify the type of a class in the JSON output.
 * @param name  Name of the type.
 */
// TODO: Better via reflection?
abstract class TypeName(name: String){
  def typeName = name
}



/**
 * Source code sample.
 * @param srcPos        Source position.
 * @param srcCodeLines  Sorrouning source code. 
 */
case class SourceSample(
    srcPos: ERangePositionWithTokenId,
    srcCodeLines: List[String]){}


/**
 * Basic information type.
 */
trait EntityInfo {
  def name: String 
  def members: Iterable[EntityInfo]
}


case class PackageInfo(
    name: String,
    fullName: String,
    members: Seq[EntityInfo]
) extends TypeName("PackageInfo") with EntityInfo

case class NamedTypeMemberInfo(
    name: String,
    tpe: TypeInfo,
    pos: Option[SourcePosition],
    signatureString: Option[String],
    declAs: DeclaredAs
) extends TypeName("NamedTypeMemberInfo") with EntityInfo {
  override def members = List.empty
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
) extends TypeName("BasicTypeInfo") with TypeInfo 

case class ArrowTypeInfo(
    name: String,
    typeId: Int,
    resultType: TypeInfo,
    paramSections: Iterable[ParamSectionInfo]
) extends TypeName("ArrowTypeInfo") with TypeInfo {
  def declAs = DeclaredAs.Nil
  def fullName = name
  def typeArgs = List.empty
  def members = List.empty
  def pos = None
  def outerTypeId = None
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
)


trait ImplicitInfo

case class ImplicitConversionInfo(
  start: Int,
  end: Int,
  fun: SymbolInfo
) extends TypeName("ImplicitConversionInfo") with ImplicitInfo 

case class ImplicitConversionInfoSummary(
  fun: SymbolInfo
) extends TypeName("ImplicitConversionInfoSummary") with ImplicitInfo


case class ImplicitParamInfo(
  start: Int,
  end: Int,
  fun: SymbolInfo,
  params: List[SymbolInfo],
  funIsImplicit: Boolean
) extends TypeName("ImplicitParamInfo") with ImplicitInfo

case class ImplicitParamInfoSummary(
  fun: SymbolInfo,
  params: List[SymbolInfo],
  funIsImplicit: Boolean
) extends TypeName("ImplicitParamInfoSummary") with ImplicitInfo 

