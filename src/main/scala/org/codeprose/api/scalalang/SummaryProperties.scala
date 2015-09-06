package org.codeprose.api.scalalang

import org.codeprose.api.SourcePosition

case class SourceSample(
    srcPos: ERangePositionWithTokenId,
    srcCodeLines: List[String]){}


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


trait EntityInfo {
  def name: String 
  def members: Iterable[EntityInfo]
  def _infoType: String
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


trait ImplicitInfo

case class ImplicitConversionInfo(
  start: Int,
  end: Int,
  fun: SymbolInfo
) extends ImplicitInfo {
  def _infoType = "ImplicitConversionInfo"
}

case class ImplicitConversionInfoSummary(
  fun: SymbolInfo
) extends ImplicitInfo {
  def _infoType = "ImplicitConversionInfoSummary"
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

case class ImplicitParamInfoSummary(
  fun: SymbolInfo,
  params: List[SymbolInfo],
  funIsImplicit: Boolean
) extends ImplicitInfo {
  def _infoType = "ImplicitParamInfoSummary"
}

