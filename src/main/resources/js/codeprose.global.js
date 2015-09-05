// Codeprose

// Tooltip helpers

function tooltipSrcFile_getLinkToDeclaration(elem){
	var rawLinkToDeclaration = $(elem).data("cp-declaration");
        var linkToDeclaration = ""
        if(rawLinkToDeclaration){
          console.log( rawLinkToDeclaration);
          linkToDeclaration = "<a href='" + rawLinkToDeclaration + "'>Declaration</a>" + "<br/>";
        }
	return linkToDeclaration;
}

function tooltipSrcFile_getLinkToTypeOverview(typeId){
	var retString = "";
	var typeInspectInfo = typeInformation[typeId];

	if(typeInspectInfo != null){
		var typeInfo = typeInspectInfo.tpe;
		if(typeInfo._infoType === "BasicTypeInfo"){
			var rawLink = "../" + "typeInformationSummary.html#TYPEID" + typeId;
			retString = "<div style='margin-top:0.5em;'>" + "<a href='" + rawLink + "'>" + "Type overview" + "</a>" + "</div>";
		}
	}
	return retString;
}

function tooltipSrcFile_getLinkToWhereTypeUsedInProject(typeId){

  	var linkToWhereUsedInProject = "";
        if(typeId != null){
          rawLinkToWhereUsedProject = "../whereUsedSummary.html"+ "#TYPEID" + typeId
          linkToWhereUsedInProject = "<div style='margin-top:0.5em;'>" + "<a href='" + rawLinkToWhereUsedProject + "'>Type where used in project</a>" + "</div>";
        }    

	return linkToWhereUsedInProject;
}


function tooltipSrcFile_getImplicitInformation(elem) {
	return "";//"<ul style='margin-left:1em;'><li>Implicit TODO</li></ul>";
}


/*
 * Return a type Inspect summary unordered list.
 *
 */
function tooltipSrcFile_getTypeInformation(typeInspectInfo){
	var retString = "";
	if(typeInspectInfo ==  null){
		return retString;
	}
	retString += "<div style='margin-top:0.5em;'>";
	if(typeInspectInfo.tpe._infoType === "BasicTypeInfo"){
		retString += tooltipSrcFile_getTypeInformation_BasicTypeInfo(typeInspectInfo);
        } else {
		retString += tooltipSrcFile_getTypeInformation_ArrowTypeInfo(typeInspectInfo);
	}
	retString += "</div>";	
    
	return retString;
}

function tooltipSrcFile_getTypeInformation_ArrowTypeInfo(typeInspectInfo){

	var retString ="";
	
	retString += "Arrow information:";
	retString += "<ul style='padding-left:2em;'>"
	// Parameters
	if(typeInspectInfo.tpe.paramSections != null){
		var parameterList = tooltipSrcFile_getListOfParameters(typeInspectInfo.tpe.paramSections);
		if(parameterList != ""){
	 		retString += "<li>Parameters:" + parameterList + "</li>";
		}
	}
	
	// Return type
	if(typeInspectInfo.tpe.resultType != null){

		var resultType = tooltipSrcFile_getResultType(typeInspectInfo.tpe.resultType);

		if(resultType != ""){
	 		retString += "<li style='margin-top:0.5em;'>Result type:" + resultType + "</li>";
		} else {
		}


	}

	retString += "</ul>"
	return retString;
}

function tooltipSrcFile_getResultType(typeInfo){
	var retString = "";
	var retTypeName = getTypeInfoName(typeInfo);
	var retTypeId = typeInfo.typeId;
	var retTypeInspectInfo = typeInformation[retTypeId];
	console.log(retTypeInspectInfo);
	if(retTypeInspectInfo != null && retTypeInspectInfo.tpe._infoType === "BasicTypeInfo"){
		
		var rawLink = getRawLinkToTypeDefinition(retTypeInspectInfo.tpe); 
		var declAs = typeInfo.declAs;
		if(rawLink!=""){
			retString += "<ul style='margin-top:0.5em;padding-left:1em;'>" + "<li style='margin-top:0.2em;'>" + declAs + " - " + "<a href='.." + rawLink + "'>" + retTypeName + "</a>" + "</li>" + "</ul>";
		} else {
			retString += "<ul style='margin-top:0.5em;padding-left:1em;'>" + "<li style='margin-top:0.2em;'>" + declAs + " - " + retTypeName + "</li>" + "</ul>";
}
	} else {
		retString += "<ul style='margin-top:0.5em;padding-left:1em;'>" + "<li style='margin-top:0.2em;'>" + retTypeName + "</li>" + "</ul>";

	}

return retString;

}

function tooltipSrcFile_getListOfParameters(paramSections){
	var retString = "<ul style='margin-top:0.5em;padding-left:1em;'>";
	if(paramSections.length==0){
		retString += "<li style='margin-top:0.2em;'>" + "-" + "</li>";
	} else {
		for(var i=0;i<paramSections.length;i++){
			var params = paramSections[i];
		
	 		var implicitInd = "";
			if(params.isImplicit == true){ 
			  implicitInd = " (impl)";
			}

			if(params.params.length==0){
				retString += "<li style='margin-top:0.2em;'>" + "-" + "</li>";
			} else {

				for(var k=0;k<params.params.length;k++){
					var paramName = params.params[k][0];
					var paramTypeInfo = params.params[k][1];
			 		
			  		var paramTypeName = getTypeInfoName(paramTypeInfo);
	
					var rawLinkToDef = "";
					if(paramTypeInfo._infoType === "BasicTypeInfo"){
						var typeInspectInfo = typeInformation[paramTypeInfo.typeId];
						if(typeInspectInfo != null){
							rawLinkToDef = getRawLinkToTypeDefinition(typeInspectInfo.tpe);
						}
					}
					if(rawLinkToDef!= ""){
						retString += "<li style='margin-top:0.2em;'>" + paramName + ": " + "<a href='.." + rawLinkToDef + "'>" + paramTypeName + "</a>" + implicitInd +"</li>";
					} else {
						retString += "<li style='margin-top:0.2em;'>" + paramName + ": " + paramTypeName + implicitInd +"</li>";
					}
	
				
				}
			}
		}
	}
	retString += "</ul>";
	return retString;
}


function tooltipSrcFile_getTypeInformation_BasicTypeInfo(typeInspectInfo){
var retString = "";
retString += "Type information:" + "<ul style='margin-left:0em;padding-left:2em;'>";

		var declaredAs = typeInspectInfo.tpe.declAs;
		if(declaredAs != null){
		//	retString += "<li> Declared as: " + declaredAs + "</li>";
		}

		var rawLinkToTypeDef = getRawLinkToTypeDefinition(typeInspectInfo.tpe);	

		if(rawLinkToTypeDef != ""){
			retString += "<li>" + declaredAs + " - " + "<a href='.." + rawLinkToTypeDef + "'>Definition</a>"  + "<li/>";
		} else {
			retString += "<li>" + declaredAs + " - Def. outside project" + "</li>"; 
		}
	
		// Implemented interfaces:
		var interfaceList = tooltipSrcFile_getListOfInterfaces(typeInspectInfo.interfaces);
		if(interfaceList != ""){
			retString += "<li style='margin-top:0.5em;'>" + "Implements:" + interfaceList + "</li>";	
		}
			
		

		// Outer type
		if(typeInspectInfo.tpe.outerTypeId!=null){
			var outerTypeId = typeInspectInfo.tpe.outerTypeId;
			var outerTypeInspectInfo = typeInformation[outerTypeId];
			if(outerTypeInspectInfo.tpe != null && outerTypeInspectInfo.tpe._infoType === "BasicTypeInfo"){
			
				var outerTypeList = tooltipSrcFile_getOuterTypeInformation(outerTypeInspectInfo.tpe);
				retString += "<li style='margin-top:0.5em;'>" + "Outer type: " + outerTypeList + "</li>";
			}
		}

		// Companion 
		if(typeInspectInfo.companionId != null){

			var companionInfo = tooltipSrcFile_getCompanionObjectInformation(typeInspectInfo);
			if(companionInfo != ""){
				retString += "<li style='margin-top:0.5em;'>" + "Companion: " + companionInfo + "</li>";
			}
		} 

		retString += "</ul>";
		
	return retString;
}


function tooltipSrcFile_getCompanionObjectInformation(typeInspectInfo){
 	var retString = "";
	if(typeInspectInfo.companionId != null){
			
		var companionInspectInfo = typeInformation[typeInspectInfo.companionId];
		if(companionInspectInfo != null && companionInspectInfo.tpe._infoType === "BasicTypeInfo") {
			var typeInfo = companionInspectInfo.tpe;
			var declAs = typeInfo.declAs;
			var n = getTypeInfoName(typeInfo);
			var rawLink = getRawLinkToTypeDefinition(typeInfo);
			
			if(rawLink!=""){
				retString += "<ul style='margin-left:0em;padding-left:1em;'><li style='margin-top:0.5em;'>" + declAs + " - " + "<a href='.." + rawLink + "'>"+ n + "</a></li></ul>";
			} else {
				retString += "<ul style='margin-left:0em;padding-left:1em;'><li style='margin-top:0.5em;'>" + declAs + " - " + n + "</li></ul>";
			}

		}
	} 

	return retString;
}



function tooltipSrcFile_getOuterTypeInformation(basicTypeInfo){
	console.log(basicTypeInfo);
	var retString = "";
	var outerTypeName = getTypeInfoName(basicTypeInfo);
	var outerTypeRawLink = getRawLinkToTypeDefinition(basicTypeInfo);
	var outerTypeDeclAs = basicTypeInfo.declAs;
	
	if(outerTypeRawLink!=""){
		retString += "<ul style='margin-left:0em;padding-left:1em;'><li style='margin-top:0.5em;'>" + outerTypeDeclAs + " - " + "<a href='.." + outerTypeRawLink + "'>"+ outerTypeName + "</a></li></ul>";
	} else {
		retString += "<ul style='margin-left:0em;padding-left:1em;'><li style='margin-top:0.5em;'>" + outerTypeDeclAs + " - " + outerTypeName + "</li></ul>";
	}
	return retString;
}


/*
* Input Array of InterfaceInfo
*/
function tooltipSrcFile_getListOfInterfaces(interfaceInfos){
	var retString = "";
	if(interfaceInfos!=null && interfaceInfos.length>0){
		retString += "<ul style='margin-left:0em;padding-left:1em;'>";
		for(var i=0; i<interfaceInfos.length;i++){
			var typeInfo=interfaceInfos[i].tpe;
			
			var intFaceName = getTypeInfoName(typeInfo)	
			var intFaceDeclAs = typeInfo.declAs;
			var intFaceDefLink = getRawLinkToTypeDefinition(typeInfo);			
			if(intFaceDefLink != ""){
					retString += "<li style='margin-top:0.2em;'>" + intFaceDeclAs + " - " + "<a href='.." + intFaceDefLink + "'>" +intFaceName + "</a>" +"</li>";
			} else {
				retString += "<li  style='margin-top:0.2em;'>" + intFaceDeclAs + " - " + intFaceName + "</li>";
			}


		}
		retString += "</ul>";
	}

	return retString;

}


/*
* Returns 
*	if type defintion in project: "/content/srcFile.scala.html#T<ID>"
*	else "" 
* 
*
*/
function getRawLinkToTypeDefinition(basicTypeInfo){

	var rawLink = "";
	if(basicTypeInfo!=null && basicTypeInfo.pos != null){
		var srcFileLink = srcFileToRelLink[basicTypeInfo.pos.filename];
	        var tokenId = basicTypeInfo.pos.tokenId;
		
		if(srcFileLink != null && tokenId != -1){
			rawLink = srcFileLink + "#T" + tokenId;		
		}
	}

	return rawLink;
}


function getTypeInfoName(typeInfo){
	var n = "";
	if(typeInfo._infoType === "BasicTypeInfo"){
		n=typeInfo.fullName;	
	} else {
		n=typeInfo.name;
	}
	return n;
}
