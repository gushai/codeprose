package org.codeprose.api.scalalang

import org.codeprose.api.SourcePosition


/*
 * Contains Scala specific classes to enrich a Scala project.
 */

/*
 * The elements are copied (with minor adjustments) from org.ensime.api.outgoing.scala. 
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
 * Not used directly! Only derived classes are used.
 * @param name    Name of the type.
 * @param member  Members of the entity.
 */
trait EntityInfo {
  def name: String 
  def members: Iterable[EntityInfo]
}

/**
 * Summarizes a package.
 * @param name      Package name.
 * @param fullName  Full package name.
 * @param members   Package members.
 */
case class PackageInfo(
    name: String,
    fullName: String,
    members: Seq[EntityInfo]
) extends TypeName("PackageInfo") with EntityInfo

/**
 * Summarizes a named type member.
 * @param name            Name.
 * @param tpe             Type information.
 * @param pos             Source position.
 * @param signatureString Signature string of the type.
 * @param declAs          Declared as.
 */
case class NamedTypeMemberInfo(
    name: String,
    tpe: TypeInfo,
    pos: Option[SourcePosition],
    signatureString: Option[String],
    declAs: DeclaredAs
) extends TypeName("NamedTypeMemberInfo") with EntityInfo {
  override def members = List.empty
}

/**
 * Summarizes a type.
 * Note used directly. Only derived classes are used.
 * @param   name        Name.
 * @param   typeId      Internal id referencing the type.
 * @param   declAs      Declared as.
 * @param   fullName    Full name.
 * @param   typeArgs    Type arguments
 * @param   members     Members.
 * @param   pos         Source position of definition.
 * @param   outerTypeId Internal id of owning type.
 */
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

/**
 * Summarizes objec/trait/class.
 * @param   name        Name.
 * @param   typeId      Internal id referencing the type.
 * @param   declAs      Declared as.
 * @param   fullName    Full name.
 * @param   typeArgs    Type arguments
 * @param   members     Members.
 * @param   pos         Source position of definition.
 * @param   outerTypeId Internal id of owning type.
 */
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

/**
 * Summarizes methods/functions.
 * @param   name          Name.
 * @param   typeId        Internal id referencing the type.
 * @param   resultType    Internal id of result type.
 * @param   paramSections Information in parameters by section.
 */
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

/**
 * Summarizes a parameter section.
 * @param params      Iterable of parameter name and type information.
 * @param isImplicit  Indicator is parameter section implicit.
 */
case class ParamSectionInfo(
  params: Iterable[(String, TypeInfo)],
  isImplicit: Boolean
)

/**
 * Summarizes an interface.
 * @param tpe     Type information.
 * @param viaView ???
 */
case class InterfaceInfo(
    tpe: TypeInfo,
    viaView: Option[String]
)

/**
 * Detailed information on a type.
 * @param tpe         Type information.
 * @param companionId Internal id of the companion object.
 * @param interfaces  Iterable of the interfaces implemented by the type.
 */
case class TypeInspectInfo(
    tpe: TypeInfo,
    companionId: Option[Int],
    interfaces: Iterable[InterfaceInfo]
)


/**
 * Basis for the implicit information.
 * Not used directly! Only derived classes are used.
 */
trait ImplicitInfo

/**
 * Summarizes a implicit conversion.
 * @param start Offset start of symbol to which the implicit conversion applies.
 * @param end   Offset end of symbol to which the implicit conversion applies.
 * @param fun   Symbol information.
 */
case class ImplicitConversionInfo(
  start: Int,
  end: Int,
  fun: SymbolInfo
) extends TypeName("ImplicitConversionInfo") with ImplicitInfo 

/**
 * Summarizes a implicit conversion.
 * @param fun   Symbol information.
 */
case class ImplicitConversionInfoSummary(
  fun: SymbolInfo
) extends TypeName("ImplicitConversionInfoSummary") with ImplicitInfo

/**
 * Summarizes a implicit parameter.
 * @param start         Offset start of symbol to which the implicit conversion applies.
 * @param end           Offset end of symbol to which the implicit conversion applies.
 * @param fun           Symbol information.
 * @param param         Parameter information.
 * @param funIsImplicit Indicator ...
 */
case class ImplicitParamInfo(
  start: Int,
  end: Int,
  fun: SymbolInfo,
  params: List[SymbolInfo],
  funIsImplicit: Boolean
) extends TypeName("ImplicitParamInfo") with ImplicitInfo


/**
 * Summarizes a implicit parameter.
 * @param fun           Symbol information.
 * @param param         Parameter information.
 * @param funIsImplicit Indicator ...
 */
case class ImplicitParamInfoSummary(
  fun: SymbolInfo,
  params: List[SymbolInfo],
  funIsImplicit: Boolean
) extends TypeName("ImplicitParamInfoSummary") with ImplicitInfo 

