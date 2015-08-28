package org.codeprose.util

import org.codeprose.api.TypeInformation
import org.codeprose.api.SourcePosition
import org.codeprose.api.TypeInfo
import org.codeprose.api.EntityInfo
import org.codeprose.api.BasicTypeInfo
import org.codeprose.api.ArrowTypeInfo
import org.codeprose.api.ParamSectionInfo
import org.codeprose.api.DeclaredAs
import org.codeprose.api.OffsetSourcePositionWithTokenId
import org.codeprose.api.TypeInspectInfo
import org.codeprose.api.TypeInspectInfo
import org.codeprose.api.InterfaceInfo
import org.codeprose.api.SymbolInfo
import org.codeprose.api.PackageInfo
import org.codeprose.api.NamedTypeMemberInfo




object EnsimeApiToCodeproseApi {
  
  // TODO: Delete - Beg
  
  def TypeInspectInfoToTypeInformation(typeInspectInfo: Option[org.ensime.api.TypeInspectInfo]) : Option[TypeInformation] = {

    typeInspectInfo match {
      case Some(tII) => {
      val typeId = tII.`type`.typeId
      val fullname = tII.`type`.fullName
      val interfaces = tII.supers.map ( e => { e.tpe.fullName } ).toList
    
    Some(new TypeInformation(typeId,fullname,interfaces))    
      }
      case None => None
    }
     
  }

  // Delete - end
  
  def convertToSymbolInfo(symInfo: org.ensime.api.SymbolInfo) : SymbolInfo = {
    val tpe = convertToTypeInfo(symInfo.`type`)
    val declPos = symInfo.declPos match {
          case Some(p) => { convertToSourcePosition(p) }
          case None => { None }
        }
    CodeproseApiCreator.SymbolInfo(symInfo.name, symInfo.localName, declPos, tpe, symInfo.isCallable, symInfo.ownerTypeId)
  }
  
  
  def convertToTypeInfo(typeInfo: org.ensime.api.TypeInfo) : TypeInfo = {
    
    val tpeInfo = typeInfo match {
      case typeInfo: org.ensime.api.BasicTypeInfo => {
        
        val srcPos = typeInfo.pos match {
          case Some(p) => { convertToSourcePosition(p) }
          case None => { None }
        }
       
        val typeArgs = if(typeInfo.typeArgs == List.empty){
          List[TypeInfo]()
        } else {
          typeInfo.typeArgs.map(t => convertToTypeInfo(t))
        }
          
                                               
        val members =  if(typeInfo.members == List.empty){
          List[EntityInfo]()
        } else {
          typeInfo.members.map(e => convertToEntityInfo(e))
        }
        
        val declaredAt = convertDelaredAs(typeInfo.declAs)
        
        CodeproseApiCreator.BasicTypeInfo(typeInfo.name,typeInfo.typeId,declaredAt,typeInfo.fullName,typeArgs,members,srcPos,typeInfo.outerTypeId)
      }
      case tree : org.ensime.api.ArrowTypeInfo => {
        
        val resultType = convertToTypeInfo(tree.resultType)
        val paramSections = if(tree.paramSections == List.empty) {
          List[ParamSectionInfo]()
        } else {
          tree.paramSections.map(e=>convertToParamSectionInfo(e))
        }
        
        
        CodeproseApiCreator.ArrowTypeInfo(tree.name,tree.typeId,resultType,paramSections) 
      }
    } 
    tpeInfo
  }
   
  def convertToSourcePosition(srcPos: org.ensime.api.SourcePosition) : Option[OffsetSourcePositionWithTokenId] = {
    srcPos match {
      case pos : org.ensime.api.OffsetSourcePosition => {
        val tokenId = -1
        Some(OffsetSourcePositionWithTokenId(pos.file.getAbsolutePath,pos.offset,tokenId))
      } 
      case _ => { None }
    }
  }
 
  def convertToParamSectionInfo(paramSection: org.ensime.api.ParamSectionInfo) : ParamSectionInfo = {
    val params = if(paramSection.params == List.empty){
      List[(String,TypeInfo)]()
    } else {
      paramSection.params.map(e => (e._1,convertToTypeInfo(e._2)))
      }
    CodeproseApiCreator.ParamSectionInfo(params, paramSection.isImplicit)
    
  }
  
  def convertToTypeInspectInfo(typeInspectInfo: org.ensime.api.TypeInspectInfo) : TypeInspectInfo = {
    val tpe = convertToTypeInfo(typeInspectInfo.`type`)
    val interfaces = if(typeInspectInfo.interfaces == List.empty){
      List[InterfaceInfo]()
    } else {
      typeInspectInfo.interfaces.map(e=>convertToInterfaceInfo(e))
      }
    CodeproseApiCreator.TypeInspectInfo(tpe, typeInspectInfo.companionId, interfaces)
  }
  
  def convertToInterfaceInfo(interfaceInfo: org.ensime.api.InterfaceInfo) : InterfaceInfo = {
    val tpe = convertToTypeInfo(interfaceInfo.`type`)
    CodeproseApiCreator.InterfaceInfo(tpe, interfaceInfo.viaView)
  }
  
  def convertDelaredAs(declAs: org.ensime.api.DeclaredAs) : DeclaredAs = {
    declAs match {
      case org.ensime.api.DeclaredAs.Method => DeclaredAs.Method
      case org.ensime.api.DeclaredAs.Trait => DeclaredAs.Trait
      case org.ensime.api.DeclaredAs.Interface => DeclaredAs.Interface
      case org.ensime.api.DeclaredAs.Object => DeclaredAs.Object
      case org.ensime.api.DeclaredAs.Class => DeclaredAs.Class
      case org.ensime.api.DeclaredAs.Field => DeclaredAs.Field
      case org.ensime.api.DeclaredAs.Nil => DeclaredAs.Nil
    }
  }
  
  def convertToPackageInfo(packInfo: org.ensime.api.PackageInfo) : PackageInfo = {
    val members = if(packInfo.members == List.empty){
      List[EntityInfo]()
    } else {packInfo.members.map(e=>convertToEntityInfo(e))
      }
    CodeproseApiCreator.PackageInfo(packInfo.name, packInfo.fullName, members)
  }
  
  def convertToNamedTypeMemberInfo(namedTypeMemInfo: org.ensime.api.NamedTypeMemberInfo) : NamedTypeMemberInfo = {
    val tpe = convertToTypeInfo(namedTypeMemInfo.`type`)
    val pos = namedTypeMemInfo.pos match {
          case Some(p) => { convertToSourcePosition(p) }
          case None => { None }
        }
    val declAs = convertDelaredAs(namedTypeMemInfo.declAs)
    CodeproseApiCreator.NamedTypeMemberInfo(namedTypeMemInfo.name,tpe, pos, namedTypeMemInfo.signatureString, declAs)
  }
  
  
  def convertToEntityInfo(entityInfo: org.ensime.api.EntityInfo) : EntityInfo = {
   val eInfo = entityInfo match {
      case namedTypeMemInfo : org.ensime.api.NamedTypeMemberInfo => {
        convertToNamedTypeMemberInfo(namedTypeMemInfo)
      }
      case packInfo : org.ensime.api.PackageInfo => {
       convertToPackageInfo(packInfo) 
      }
      case typeInfo : org.ensime.api.TypeInfo => {
        convertToTypeInfo(typeInfo)
      }
      
    }
   eInfo
  }
  
}




object CodeproseApiCreator extends CodeproseApiCreator

trait CodeproseApiCreator {
  
  def BasicTypeInfo( 
    name: String,
    typeId: Int,
    declAs: DeclaredAs,
    fullName: String,
    typeArgs: Iterable[TypeInfo],
    members: Iterable[EntityInfo],
    pos: Option[SourcePosition],
    outerTypeId: Option[Int]) : TypeInfo = {
   
    new BasicTypeInfo(name,typeId,declAs,fullName,typeArgs,members,pos,outerTypeId)
  }
  
  def ArrowTypeInfo(
    name: String,
    typeId: Int,
    resultType: TypeInfo,
    paramSections: Iterable[ParamSectionInfo]) : TypeInfo = {
    
    new ArrowTypeInfo(name,typeId,resultType,paramSections)
  }
  
  def ParamSectionInfo(
      params: Iterable[(String, TypeInfo)],
      isImplicit: Boolean) : ParamSectionInfo = {
    new ParamSectionInfo(params, isImplicit)
  }
  
   def TypeInspectInfo(tpe: TypeInfo,
                      companionId: Option[Int],
                      interfaces: Iterable[InterfaceInfo]) : TypeInspectInfo = {
    new TypeInspectInfo(tpe,companionId,interfaces)
  }
  
  def InterfaceInfo(tpe: TypeInfo,
                    viaView: Option[String]) : InterfaceInfo = {
    new InterfaceInfo(tpe,viaView)
  }
  
  def SymbolInfo(  
      name: String,
      localName: String,
      declPos: Option[SourcePosition],
      tpe: TypeInfo,
      isCallable: Boolean,
      ownerTypeId: Option[Int]) : SymbolInfo = {
    new SymbolInfo(name,localName,declPos,tpe,isCallable,ownerTypeId)
  }
  
  
  def PackageInfo( name: String, fullName: String, members: Seq[EntityInfo]) : PackageInfo = {
    new PackageInfo(name,fullName, members)
  }
  
  def NamedTypeMemberInfo(    
      name: String,
      tpe: TypeInfo,
      pos: Option[SourcePosition],
      signatureString: Option[String],
      declAs: DeclaredAs) : NamedTypeMemberInfo = {
        new NamedTypeMemberInfo(name, tpe, pos, signatureString, declAs)
  }
  
  
}




  


 


 

  
  


  
