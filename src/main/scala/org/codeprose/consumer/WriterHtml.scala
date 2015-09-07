package org.codeprose.consumer

import java.io.File
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.ArrayBuffer
import spray.json._
import org.codeprose.api._
import org.codeprose.api.scalalang._
import org.codeprose.consumer.util.CommentUtil
import org.codeprose.consumer.util.MarkdownConverter
import org.codeprose.consumer.util.OutputContextSetter
import org.codeprose.util.FileUtil
import org.codeprose.util.StringUtil
import org.codeprose.util.CodeproseJsonFormat._


class ResourceRelPaths(val base: String, val target: String)



class WriterContextHtml(
    val outputMainPath: File,
    val verbose: Boolean
    ) extends ConsumerContext(verbose) {  
  
  val outputRelFolders = List("content","js","style")

  val resourcesToCopy = List[ResourceRelPaths](
      new ResourceRelPaths("/html/style.css","style/style.css"),
      new ResourceRelPaths("/js/codeprose.global.js","js/codeprose.global.js")
      )

  val summaryFilesRelPath = Map("summary.index" -> "/index.html",
                                "summary.typeinfo" -> "/typeInformationSummary.html",
                                "summary.packageinfo" -> "/packageSummary.html",
                                "summary.whereUsed" -> "/whereUsedSummary.html",
                                "js.global.typeinfo" -> "/js/codeprose.typeinformation.js",
                                "js.global.whereusedinfo" -> "/js/codeprose.whereusedinformation.js",
                                "js.global.packageinfo" -> "/js/codeprose.packageinformation.js",
                                "js.global.helper" -> "/js/codeprose.helper.js",
                                "js.global.implicitinfo" -> "/js/codeprose.implicitinformation.js")
}



class WriterHtml(implicit c: WriterContextHtml) extends Consumer with LazyLogging {
 
  def initialize() : Unit = {}
  def close() : Unit = {}
  
	def generateOutput(projectInfo: ProjectInfo) : Unit = {
    
    val htmlOutputContext = new HtmlOutputContext(c.outputMainPath,projectInfo.enrichedTokens.map(e => e._1).toList)
    
    setupOutputEnvironment()
    
    generateGlobalJSInformationFiles(projectInfo.summary,htmlOutputContext)
    
    generateSummaryPages(projectInfo,htmlOutputContext)
    
    generateIndividualPages(projectInfo,htmlOutputContext)
    
   
	}

  
  /**
   * Generates and writes to disk the projects summary pages (incl. javascript 'databases') 
   * @param projectInfo ProjectInfor
   * 
   */
  private def generateSummaryPages(projectInfo: ProjectInfo, htmlOutputContext: HtmlOutputContext) : Unit = {
   
    logger.info("Generating summary files ... ")
    generateIndexPage(projectInfo, htmlOutputContext)
    generateWhereUsedPage(projectInfo, htmlOutputContext)
    generateTypeInformationPage(projectInfo, htmlOutputContext)
    generatePackageInformationPage(projectInfo, htmlOutputContext)
     
        
  }
  
  /**
   * Generates and saves the index page to disk.
   * @param projectInfo       Project information
   * @param htmlOutputContext Output context.
   */
  private def generateIndexPage(
      projectInfo: ProjectInfo, 
      htmlOutputContext: HtmlOutputContext) : Unit = { 
    
    val relFileName = c.summaryFilesRelPath.get("summary.index")
    
    if(relFileName.isDefined){
    
      logger.info("\t" + "index file " + relFileName.get)
    
      val outputFilename= new File(c.outputMainPath.getAbsolutePath + relFileName.get)
         
      val htmlContext = new HtmlSummaryFileContext()      
    
      val srcFilenames = htmlOutputContext.srcFiles
      val labels = htmlOutputContext.filenamesShortened
      val links = htmlOutputContext.relativeOutputFilenames.map(e=>"."+e)
      
      val indexFileTitle = "Overview"
      
      // File listing
      
      val filesTitle = "Files"                 
      
      val filesEntries = projectInfo.summary(ScalaLang.packageNamePerFile) match {
        case Some(packageNamesPerFile) => {
          
          val packageNamesPerFileSorted = srcFilenames.map( f => packageNamesPerFile.get(f) match {
            case Some(name) => name
            case None => ""
          })
          
          val entriesFileWithLinkAndLabel = (srcFilenames zip ( labels zip links)).map{e => (e._1,e._2._1,e._2._2)}
          val entriesByPackage = packageNamesPerFileSorted.zip(entriesFileWithLinkAndLabel).map(e=>(e._1,e._2._1,e._2._2,e._2._3)).groupBy(e=>e._1)
          
          val entires = entriesByPackage.map({ e => 
            val packName = e._1
            val fileEntries = e._2.map(f => {
              val titleText = "Package: " + f._1 +"\nSource file: " + f._2.getAbsolutePath
              s"""<li style="margin-top:0.25em;">\n""" + 
              s"""<a href="""" + f._4 + s"""" title="""" + titleText + s"""">""" + f._3 + s"""</a>""" +
              "\n</li>"
              
            }).mkString("<ul><li style='margin-top:0.8em;'>  </li>\n","\n","</ul>\n")
        
            val packageEntry = s"""<li style="margin-top:0.8em;"><b>$packName</b>""" + fileEntries + s"""</li>"""
            packageEntry
            }).mkString("<ul>\n","\n","</ul>\n")
         
          entires
        } 
        case None => {
          val entries = (srcFilenames zip ( labels zip links)).map{e => (e._1,e._2._1,e._2._2)}
          entries.map({e => s"""<li style="margin-top:0.2em;">\n<a href="""" + e._3 + s"""" title="Source file: """ + e._1 + s"""">""" + e._2 + s"""</a>\n</li>"""}).mkString("<ul>\n","\n","</ul>\n")
        }
      }
      
      
      
      val filesList = htmlContext.packageContent(filesTitle,filesEntries) 
      
      // Classes/Traits/ ... 
      
      val otherTitle = ""                 
      val otherEntries = ""     
      
      val otherList = htmlContext.packageContent(otherTitle,otherEntries)
      
      // Project summaries
      
      val projectSummariesTitle = "Project Summaries"
      val projectSummariesEntries = s"""
<ul>
<li style="margin-top:0.4em;">
<a href="./typeInformationSummary.html" title="Type information">Type information</a>
</li>
<li style="margin-top:0.4em;">
<a href="./packageSummary.html" title="Package information">Package information</a>
</li>
<li style="margin-top:0.4em;">
<a href="./whereUsedSummary.html" title="Where used information by type">Where used information</a>
</li>
</ul>"""
      val projectSummaries = htmlContext.packageContent(projectSummariesTitle,projectSummariesEntries)
      
      // create output
      val content = List(filesList,otherList,projectSummaries).mkString("\n\n")  
        
     FileUtil.writeToFile(outputFilename,htmlContext.getBegin(indexFileTitle,"",false) + content  + htmlContext.getEnd())      
    } else {
      logger.error("Unable to generate index file. No file name provided!")
    }
    
  }
  
  private def generateTypeInformationPage(projectInfo: ProjectInfo, htmlOutputContext: HtmlOutputContext) : Unit = { 
   
    
    val relFileName = c.summaryFilesRelPath.get("summary.typeinfo")
    
    if(relFileName.isDefined){
    
       logger.info("\t" + "type information \t" + relFileName.get)
       
       val outputFilename= new File(c.outputMainPath.getAbsolutePath + relFileName.get)
       
       val htmlContext = new HtmlSummaryFileContext()      
    
       val title = "Type information"
       
       val script = s"""
$$(document).ready(function(){ 
  

  function getTypeInformationSummary(){
    var domElemToAppend = "#ContentTypeInformationSummary";

    if(typeIds == null || typeIds.length==0){
      $$(domElemToAppend).append("<b>No type information found.</b>");
    } else {

  var typeInfoCollection = {};

          for(var i = 0;i<typeIds.length;i++){
        
          var currentTypeId = typeIds[i];
          var typeInfo = typeInformation[currentTypeId];

    var typeName = "";
    var isArrow = false;
    var declaredAs = "";  
    
    if(typeInfo.tpe._infoType === "BasicTypeInfo"){
        typeName+= typeInfo.tpe.fullName;
        declaredAs = typeInfo.tpe.declAs;
    } else if(typeInfo.tpe._infoType === "ArrowTypeInfo"){
        typeName+= typeInfo.tpe.name;
        isArrow=true;
        declaredAs="arrow";
    } else { 
        typeName+="-- Unknown -- ";
    }

    var tInfo = {};
      tInfo.name = typeName;
      tInfo.id = currentTypeId;
  
    if(typeInfoCollection[declaredAs]!=null){
      var tmp = typeInfoCollection[declaredAs];
      tmp.push(tInfo);  
      typeInfoCollection[declaredAs] = tmp;
    
    } else {
      typeInfoCollection[declaredAs] = [tInfo];
    }
  }
  
  $$(domElemToAppend).append("<table>");
//  $$(domElemToAppend).append("<tr>" + "<th style='width:6em;text-align:center;padding-bottom:0.4em;border-bottom:1px solid #CFCFCF;'>" + "<b>Type</b>" + "</th>" + "<th style='width:40em;text-align:center;border-bottom:1px solid #CFCFCF;'>" + "<b>Name</b>" + "</th>"+"</tr>")
  for (var key in typeInfoCollection){

    var keyHeadline = "<tr>" + "<td style='text-align:center;padding-left:1.3em;padding-top:0.8em;'><b>" + key.toString() + "</b></td>" + "<td style='padding-left:3em;'>"+ "</td>"+"</tr>";
    $$(domElemToAppend).append(keyHeadline);
  
    var typeGroup = typeInfoCollection[key];
    
    for(var i=0;i<typeGroup.length;i++){
      var id = typeGroup[i].id;
      var tpeName = typeGroup[i].name;

      var title=key;
      var tableEntry = "<tr>" + "<td style='text-align:right;padding-right:2em;padding-top:0.6em;margin-top:0.4em;'>" + "</td>" + "<td style='padding-left:3em;padding-top:0.6em;'>" + "<a href='#TYPEID" + id + "' title='" + title + "'>" + tpeName + "</a>"+ "</td>"+"</tr>";
      $$(domElemToAppend).append(tableEntry);
    }

  }
  $$(domElemToAppend).append("</table>");
}

  };
  

function getTypeInformationDetails(){ 
  var domElemToAppend = "#ContentTypeInformationDetails";
  
  if(typeIds == null || typeIds.length==0){
      $$(domElemToAppend).append("<b>No type information found.</b>");
    } else {
  for(i=0;i<typeIds.length;i++){
    var currentId = typeIds[i];
    var typeInspectInfo = typeInformation[currentId];
    //console.log(typeInspectInfo);
    var typeInfoEntry = getEntryForTypeInspectInfo(currentId,typeInspectInfo);
    
    $$(domElemToAppend).append(typeInfoEntry);
  }
  
    }
};

function getEntryForTypeInspectInfo(currentId,typeInfo){
  retString="";
  
  var typeName = "";
  var declaredAs = ""
    if(typeInfo.tpe._infoType === "BasicTypeInfo"){
      typeName+= typeInfo.tpe.fullName;
      declaredAs = typeInfo.tpe.declAs; 
    } else if(typeInfo.tpe._infoType === "ArrowTypeInfo"){
      typeName+= typeInfo.tpe.name;
    } else { 
      typeName+="-- Unknown -- ";
    }
  
  

  var headline = "<div style='margin-top:3em;'><div id='TYPEID"+ currentId +"'>" +  "<span style='font-weight:bold; font-size:1.2em;'>" + typeName + "</span>&nbsp; - " +  declaredAs+ "</div>";
  
  retString += headline ;


  for(var k=0;k<typeInfo.interfaces.length;k++){
    var interfaceEntry = getInterfaceEntry(typeInfo.interfaces[k]);
    retString += interfaceEntry;
  }

  
  retString += "</div>";
  return retString;
};


function getInterfaceEntry(intrface){

  var typeInfo = intrface.tpe;
  var retString = "";

  var typeName="";
  var declAs = "";
  if(typeInfo._infoType === "BasicTypeInfo"){
      typeName+= typeInfo.fullName;
      declAs = typeInfo.declAs;
    } else if(typeInfo._infoType === "ArrowTypeInfo"){
      typeName+= typeInfo.name;
      declAs = "arrow";
    } else { 
      typeName+="-- Unknown -- ";
   }
  
  
  var members = typeInfo.members;
  
  retString += "<div style='margin-top:2em;margin-left:2em;'>"
  retString += "<span style='font-weight:bold;'>" + typeName + "</span>&nbsp; - " + declAs + "";
  retString += "<table style='margin-top:1.5em;'>";
  for(var mem=0;mem<members.length;mem++){
    var memName = "<pre>" + members[mem].name + "</pre>";
    var memSigStr = members[mem].signatureString;

    var entry = "<tr>" + "<td style='width:20em;'>" + memName +"</td> <td>" + memSigStr + "</td>" + "</tr>";

    retString += entry;
  }

  retString += "</table>";

  retString += "</div>"
  return retString;
};


  getTypeInformationSummary();
  getTypeInformationDetails();
  
  
}); 
      
"""
      val noscriptTag = s"""<noscript><div style="margin-left:2em;">Activate JavaScript for this feature.</div></noscript>"""
      val content = htmlContext.packageContent("Summary",noscriptTag + s"""<div id="ContentTypeInformationSummary"></div>""") +
      htmlContext.packageContent("Details",noscriptTag + s"""<div id="ContentTypeInformationDetails"></div>""")
           
      // create output
        
      FileUtil.writeToFile(outputFilename,htmlContext.getBegin(title,script,true) + content + htmlContext.getEnd())      
    
    } else {
      logger.error("Unable to generate type summary file. No file name provided!")
    }          
    
  }
  
  private def generateWhereUsedPage(projectInfo: ProjectInfo, htmlOutputContext: HtmlOutputContext) : Unit = { 

    val relFileName = c.summaryFilesRelPath.get("summary.whereUsed")
    
    if(relFileName.isDefined){
    
      logger.info("\t" + "where used information \t" + relFileName.get)
      val outputFilename= new File(c.outputMainPath.getAbsolutePath + relFileName.get)
         
      val htmlContext = new HtmlSummaryFileContext()      
    
      val title = "Where Used by Type Id"   
      
      val script =s"""
 
  $$(document).ready(function(){ 
  


  /*
   * Creates a table with type id and name.
     * Type names link to the where used entries below.
   */
  function getWhereUsedOverview() {
    var domElemToAppend = "#ContentWhereUsedWithSourceSamplesSummary";

    if(typeIds == null || typeIds.length==0){
      $$(domElemToAppend).append("<b>No type information found.</b>");
    } else {

  var typeInfoCollection = {};

          for(var i = 0;i<typeIds.length;i++){
        
          var currentTypeId = typeIds[i];
          var typeInfo = typeInformation[currentTypeId];
        
    var typeName = "";
    var isArrow = false;
    var declaredAs = "";  
    
    if(typeInfo.tpe._infoType === "BasicTypeInfo"){
        typeName+= typeInfo.tpe.fullName;
        declaredAs = typeInfo.tpe.declAs;
    } else if(typeInfo.tpe._infoType === "ArrowTypeInfo"){
        typeName+= typeInfo.tpe.name;
        isArrow=true;
        declaredAs="arrow";
    } else { 
        typeName+="-- Unknown -- ";
    }

    var tInfo = {};
      tInfo.name = typeName;
      tInfo.id = currentTypeId;
  
    if(typeInfoCollection[declaredAs]!=null){
      var tmp = typeInfoCollection[declaredAs];
      tmp.push(tInfo);  
      typeInfoCollection[declaredAs] = tmp;
    
    } else {
      typeInfoCollection[declaredAs] = [tInfo];
    }
  }
  
      $$(domElemToAppend).append("<table>");
//$$(domElemToAppend).append("<tr>" + "<th style='width:6em;text-align:center;padding-bottom:0.4em;border-bottom:1px solid #CFCFCF;'>" + "<b>Type</b>" + "</th>" + "<th style='width:40em;text-align:center;border-bottom:1px solid #CFCFCF;'>" + "<b>Name</b>" + "</th>"+"</tr>")
  for (var key in typeInfoCollection){

    var keyHeadline = "<tr>" + "<td style='text-align:center;padding-left:1.3em;padding-top:0.8em;'><b>" + key.toString() + "</b></td>" + "<td style='padding-left:3em;'>"+ "</td>"+"</tr>";
    $$(domElemToAppend).append(keyHeadline);
  
    var typeGroup = typeInfoCollection[key];
    
    for(var i=0;i<typeGroup.length;i++){
      var id = typeGroup[i].id;
      var tpeName = typeGroup[i].name;

      var title=key;
      var tableEntry = "<tr>" + "<td style='text-align:right;padding-right:2em;padding-top:0.6em;margin-top:0.4em;'>" + "</td>" + "<td style='padding-left:3em;padding-top:0.6em;'>" + "<a href='#TYPEID" + id + "' title='" + title + "'>" + tpeName + "</a>"+ "</td>"+"</tr>";
      $$(domElemToAppend).append(tableEntry);
    }

  }
  $$(domElemToAppend).append("</table>");
}
  };
  
  function getWhereUsedDetails(){
    
    var domElemToAppend = "#ContentWhereUsedWithSourceSampleDetails";

    if(typeIds == null || typeIds.length==0){
      $$(domElemToAppend).append("<b>No type information found.</b>");
    } else {
      
      // For all type ids
      for(var i = 0;i<typeIds.length;i++){
        
        var currentTypeId = typeIds[i];
        var whereUsedInfo = whereUsedInformation[currentTypeId];
        var typeInfo = typeInformation[currentTypeId];

    var typeName = "";
    if(typeInfo.tpe._infoType === "BasicTypeInfo"){
      typeName+= typeInfo.tpe.fullName;
    } else if(typeInfo.tpe._infoType === "ArrowTypeInfo"){
      typeName+= typeInfo.tpe.name;
    } else { 
      typeName+="-- Unknown -- ";
    }

        $$(domElemToAppend).append("<div style='margin-top:3em;' id='TYPEID" + currentTypeId +"'><b>" + typeName + "</b>" + "&nbsp;&nbsp;&nbsp;&nbsp;" + "(Type Id: " +  currentTypeId + ")</div>");


        if(whereUsedInfo == null || whereUsedInfo.length==0){
          $$(domElemToAppend).append("<div style='margin-top:2em;'>" + "no where used information found" + "</div>");
        } else {

          var whereUsedExamples = getWhereUsedDetailsHandleWhereUsedEntires(whereUsedInfo);
          $$(domElemToAppend).append(whereUsedExamples);
        }
      }
    }

  };

  function getWhereUsedDetailsHandleWhereUsedEntires(whereUsedInfo) {
    var divsToAppend = "";
    // For all source positions       
    for(var srcPos=0;srcPos<whereUsedInfo.length;srcPos++){
      var filename = whereUsedInfo[srcPos].srcFilename;
      var codeSample = whereUsedInfo[srcPos].sourceSample;
      var linkToSrcFile = whereUsedInfo[srcPos].link;
      var tokenId = whereUsedInfo[srcPos].tokenId;

      divsToAppend += getWhereUsedCodeSampleDiv(filename,codeSample,linkToSrcFile,tokenId);
    }
    return divsToAppend;
  };


  function getWhereUsedCodeSampleDiv(filename,codeSample,linkToSrcFile,tokenId){
    sampleCode = ""
    if(codeSample.length>2){
                // TODO: Highlight token
                sampleCode = codeSample[0] + codeSample[1] + codeSample[2]; 
                //sampleCode = codeSample[0] + "<span style='background-color:yellow;'>"+codeSample[1] +"</span>" + codeSample[2];       
            }   
            var srcSampleDiv = "<div style='border-top:1px solid #CFCFCF;margin-top:2em;margin-left:2em;padding-top:1em;padding-left:1em;'>" + "<b>" + filename + "</b><br/><br/>" + "<div class='table-code-code'>" + "<a class='in-code' href='."+linkToSrcFile + "#T" + tokenId + "'><span style='color:black;'><pre>" + sampleCode + "</pre></span></a>" +"</div>"+"</div>";

          return srcSampleDiv;
  };

  getWhereUsedOverview();
  getWhereUsedDetails();

});     
"""
      val noscriptTag = s"""<noscript><div style="margin-left:2em;">Activate JavaScript for this feature.</div></noscript>"""
      val whereUsedSummary = htmlContext.packageContent("Summary",noscriptTag + s"""<div id="ContentWhereUsedWithSourceSamplesSummary"></div>""")
			val whereUsedDetails = htmlContext.packageContent("Details",noscriptTag + s"""<div id="ContentWhereUsedWithSourceSampleDetails"></div>""") 
      val content =  whereUsedSummary + whereUsedDetails 
      
           
      // create output
        
      FileUtil.writeToFile(outputFilename,htmlContext.getBegin(title,script,true) + content + htmlContext.getEnd())      

       
    
    } else {
      logger.error("Unable to where used summary file. No file name provided!")
    }    
  }
  
  private def generatePackageInformationPage(projectInfo: ProjectInfo, htmlOutputContext: HtmlOutputContext) : Unit = { 

    val relFileName = c.summaryFilesRelPath.get("summary.packageinfo")
    
    if(relFileName.isDefined){
    
      logger.info("\t" + "package information \t" + relFileName.get)
           
      val outputFilename= new File(c.outputMainPath.getAbsolutePath + relFileName.get)
         
      val htmlContext = new HtmlSummaryFileContext()      
    
      val title = "Package Information"   
      
      val script =s"""

$$(document).ready(function(){ 
  

  function getPackageInformationSummary(){
    var domElemToAppend = "#ContentPackgeInformationSummary";
  
  if(packageNames.length>0){
    var toAppend = "<div style='padding-left:1.3em;'>" + "<ul>";

    for(var i=0;i<packageNames.length;i++){
      var packName = packageNames[i];
      toAppend += "<li style='margin-top:0.4em;'><a href='#PACK" + packName + "'>" + packName + "</a></li>";
      
    }
    toAppend += "</ul>" + "</div>";
    $$(domElemToAppend).append(toAppend);
  } else {
    $$(domElemToAppend).append("No package information found.");
  }

  };


  function getPackageInformationDetails(){
    var domElemToAppend = "#ContentPackageInformationDetails";

  for(var i=0;i<packageNames.length;i++){

    var packName = packageNames[i] 
    var packInfo = packageInformation[packName];
    var toAppend = "<div style='padding-left:1.3em;'>";

    toAppend +="<div style='margin-top:2em; margin-bottom:0.8em;' id='PACK" + packInfo.fullName +"'><span style='font-weight:bold; font-size:1.2em;'>" + packInfo.fullName + "</span></div>";
    var memberInformation = getPackageInfoMemberDetails(packInfo.members);

toAppend += memberInformation
toAppend += "</div>";

    $$(domElemToAppend).append(toAppend);

  }

  };

function getPackageInfoMemberDetails(members){
  var retString = "";

  if(members.length>0){

// Group information

var groupedMemberInfo = {};
   
for(var mem=0;mem<members.length;mem++){
      var member = members[mem];
      
      var typeId = "";
      var declaredAs = "-";
      var nam = "";
      var linkToSrcPos = null;
      
      if(member._infoType==="BasicTypeInfo"){
        typeId = member.typeId;
        declaredAs = member.declAs;
  nam = member.fullName;
  if(member.pos!=null && member.pos.tokenId!=-1){
     linkToSrcPos = "." + srcFileToRelLink[member.pos.filename] + "#T"+ member.pos.tokenId; 
  }
      } else if(member._infoType==="ArrowTypeInfo"){
  typeId = member.typeId.toString;
        declaredAs = "arrow";
  nam = member.name;
      } else if(member._infoType==="PackageInfo"){
        declaredAs = "package";
  nam = member.fullName;
      } else {}

      var memInfo = {}
  memInfo.id = typeId;
  memInfo.declaredAs = declaredAs;      
  memInfo.name = nam;
  memInfo.linkToSrcPos = linkToSrcPos;

     if(groupedMemberInfo[declaredAs]!=null){ 
  var tmp = groupedMemberInfo[declaredAs];
  tmp.push(memInfo);     
  groupedMemberInfo[declaredAs] = tmp;
     } else {
  groupedMemberInfo[declaredAs] = [memInfo];

     }
    }
  
// Generate output
retString +=  "<table>";

for(key in groupedMemberInfo){
  var members = groupedMemberInfo[key];
  
var headline = "<tr>" + "<td style='text-align:center;padding-left:1.3em;padding-top:0.8em;'><b>" + key.toString() + "</b></td>" + "<td style='padding-left:3em;'>"+ "</td>"+"</tr>";

//var headline = "<div style='padding-left:2em;margin-top:1.5em;'>" + "<b>" + key.toString() + "</b>"
  retString += headline;
  //retString += "<ul style='padding-left:7em;margin-top:1em;'>";
  for( var i=0;i<members.length;i++){
    var mem = members[i];
    entry = "";
    tableEntry = "";
    if(mem.declaredAs === "arrow"){
tableEntry += "<tr>" + "<td style='text-align:right;padding-right:2em;padding-top:0.6em;margin-top:0.4em;'>" + "</td>" + "<td style='padding-left:3em;padding-top:0.6em;'>" + mem.name + "</a>"+ "</td>"+"</tr>";
    } else if (mem.declaredAs === "package"){
        var relLink = "#PACK" + mem.name;

tableEntry += "<tr>" + "<td style='text-align:right;padding-right:2em;padding-top:0.6em;margin-top:0.4em;'>" + "</td>" + "<td style='padding-left:3em;padding-top:0.6em;'>" + "<a href='" + relLink + "' title='" + mem.name + " - " + mem.declaredAs + "'>" + mem.name + "</a>"+ "</td>"+"</tr>";
    } else {
      if(mem.linkToSrcPos!=null){
      //entry += "<li style='margin-top:0.5em;'>" + "<a href='" + mem.linkToSrcPos + "' title='" + mem.name + " - " + mem.declaredAs + "'>" + mem.name + "</a>" + "</li>";
tableEntry += "<tr>" + "<td style='text-align:right;padding-right:2em;padding-top:0.6em;margin-top:0.4em;'>" + "</td>" + "<td style='padding-left:3em;padding-top:0.6em;'>" + "<a href='" + mem.linkToSrcPos + "' title='" + mem.name + " - " + mem.declaredAs + "'>" + mem.name + "</a>"+ "</td>"+"</tr>";
      } else {
tableEntry += "<tr>" + "<td style='text-align:right;padding-right:2em;padding-top:0.6em;margin-top:0.4em;'>" + "</td>" + "<td style='padding-left:3em;padding-top:0.6em;'>" + mem.name + "</a>"+ "</td>"+"</tr>";
}
    }

    
    retString += tableEntry;

  }
//retString +="</div></ul>";
}
  retString +="</table>";
  
  } else {
  retString += "<div style='margin-top:2em;padding-left:2em;'>No members.</div>";
  }




  return retString;
};



  getPackageInformationSummary();
  getPackageInformationDetails();
  
});         
        
        
 """
      val noscriptTag = s"""<noscript><div style="margin-left:2em;">Activate JavaScript for this feature.</div></noscript>"""
      val packageSummary = htmlContext.packageContent("Summary",noscriptTag + s"""<div id="ContentPackgeInformationSummary"></div>""")
      val packageDetails = htmlContext.packageContent("Details",noscriptTag + s"""<div id="ContentPackageInformationDetails"></div>""") 
      val content =  packageSummary + packageDetails 
      
           
      // create output
        
      FileUtil.writeToFile(outputFilename,htmlContext.getBegin(title,script,true) + content + htmlContext.getEnd())      
       
    } else {
      logger.error("Unable to package summary file. No file name provided!")
    }    
  }
 
  
  /**
   * Creates the javascript files that act as db.
   */
  private def generateGlobalJSInformationFiles(
      projectSummary: ProjectSummary, 
      htmlOutputContext: HtmlOutputContext) : Unit = {
    
    logger.info("Generating js information files ...")
    
    generateGlobalJSHelper(htmlOutputContext) 
    
    projectSummary(ScalaLang.typeInspectInformation) match {
      case Some(tpeInspectInfos) => {
        generateGlobalJSTypeInfo(tpeInspectInfos)
      } 
      case None => { logger.error("No type information provided in project summary.") }
    }
    
    
    projectSummary(ScalaLang.whereUsedByTypeIdWithCodeSample) match {
      case Some(whereUsed) => {
        generateGlobalJSWhereUsedInfo(whereUsed,htmlOutputContext)
      } 
      case None => { logger.error("No where used information provided in project summary.") }
    }
    
    
    projectSummary(ScalaLang.packageInformation) match {
      case Some(packageInfo) => {
        generateGlobalJSPackageInfo(packageInfo,htmlOutputContext)
      } 
      case None => { logger.error("No package information provided in project summary.") }
    }

    val implicitConv = projectSummary(ScalaLang.implicitConversionInformation).getOrElse(null)
    val implicitParam = projectSummary(ScalaLang.implicitParameterInformation).getOrElse(null)
    if(implicitConv != null && implicitParam!=null){
      generateGlobalJSImplicitInfo(implicitConv,implicitParam)
    } else { 
      logger.error("No implicit information provided in project summary.") 
    }
    
    
    
        
  }
  
  /**
   * Saves type information to disk in js file.
   * 
   * Included:
   *  - typeIds         Array of all type ids found
   *  - typeInformation Map of TypeInspectInfo or null
   * 
   * @param typeInfos Type information by type id.
   */
  private def generateGlobalJSTypeInfo(typeInfos: Map[Int,Option[TypeInspectInfo]]) : Unit = {
    
    val relFileName = c.summaryFilesRelPath.get("js.global.typeinfo")
    
    if(relFileName.isDefined){
      
      val outputFilename= new File(c.outputMainPath.getAbsolutePath + relFileName.get)
      logger.info("\t" + "type information: \t\t" + relFileName.get)      

      val typeIds = "typeIds = " + typeInfos.map(e=>e._1.toString).toJson.compactPrint
      val typeInformation = "typeInformation = " + typeInfos.map(e=>(e._1.toString,e._2)).toJson.compactPrint
   
      val content = List(typeIds,typeInformation).mkString("\n\n")
      FileUtil.writeToFile(outputFilename,content)      

    } else {
      logger.error("Unable to generate js file with type information. No file name provided!")
    }
    
  }
  
  
  
  
  
  /**
   * Saves where used information to disk in js file.
   * 
   * Included:
   *  - whereUsedInformation Map from type id to source postions with sample.
   * 
   * @param whereUsed Source positions by type id.
   */
  private def generateGlobalJSWhereUsedInfo(
      whereUsedWithSampleCode: Map[Int,List[(ERangePositionWithTokenId,List[String])]],
      htmlOutputContext: HtmlOutputContext): Unit = {
    
    val relFileName = c.summaryFilesRelPath.get("js.global.whereusedinfo")
    
    if(relFileName.isDefined){
      
      val outputFilename= new File(c.outputMainPath.getAbsolutePath + relFileName.get)
      logger.info("\t" + "where used information: \t" + relFileName.get)

      
       val whereUsedWithSampleCodeTranslatedSrcPositions = whereUsedWithSampleCode.map(e=>{
        val typeId = e._1.toString
        val output = e._2.map(e => {
          val srcPos = e._1
          val sample = e._2
          val srcFileNameLink = htmlOutputContext.filenamesOriginalToRelOutput.filter( e => {
            if(e._1.getAbsolutePath == srcPos.filename){
              true
            } else {
              false
            }
          }).map(e=>e._2).toList
          
          val srcFilename = htmlOutputContext.getShoretendFilename(srcPos.filename).getOrElse("")
          
          if(srcFileNameLink.size>0){
            new SourcePositionLinkWithCodeSample(srcFilename,srcFileNameLink(0),srcPos.tokenId,sample)
          } else {
            new SourcePositionLinkWithCodeSample(srcFilename,"",srcPos.tokenId,sample)
          }
        })
        (typeId,output) 
      })
      
      val whereUsedWithSampleCodeJson = "whereUsedInformation = " + whereUsedWithSampleCodeTranslatedSrcPositions.toJson.compactPrint
      
      val content = List(whereUsedWithSampleCodeJson).mkString("\n\n")
      
      FileUtil.writeToFile(outputFilename,content)      

    } else {
      logger.error("Unable to generate js file with where used information! No file name provided.")
    }
  }
  
  
  /**
   * Saves where used information to disk in js file.
   * 
   * Included:
   *  - packageNames        Array of package names
   *  - packageInformation  Map package name to package information
   * 
   * @param whereUsed Source positions by type id.
   */
  private def generateGlobalJSPackageInfo(
      packageInfo: Map[String,Option[PackageInfo]],
      htmlOutputContext: HtmlOutputContext): Unit = {
    
    val relFileName = c.summaryFilesRelPath.get("js.global.packageinfo")
    
    if(relFileName.isDefined){
      
      val outputFilename= new File(c.outputMainPath.getAbsolutePath + relFileName.get)
      logger.info("\t" + "packages information: \t" + relFileName.get)

      val packageNames = "packageNames = " + packageInfo.map(e => e._1).toList.sorted.toJson.compactPrint
      val packageInformation = "packageInformation = " + packageInfo.toJson.compactPrint
      
      val content = List(packageNames,packageInformation).mkString("\n\n")
      
      FileUtil.writeToFile(outputFilename,content)      

    } else {
      logger.error("Unable to generate js file with where used information! No file name provided.")
    }
  }
  
  
  /**
   * Saves helper information to disk in js file.
   * 
   * Included
   *  - srcFileToRelLink: Map from source file to relative output link.
   * 
   * @param htmlOutputContext HtmlOutputContext.
   */
  private def generateGlobalJSHelper(htmlOutputContext: HtmlOutputContext) : Unit = {
    
    val relFileName = c.summaryFilesRelPath.get("js.global.helper")
    
    if(relFileName.isDefined){
      
      val outputFilename= new File(c.outputMainPath.getAbsolutePath + relFileName.get)
      logger.info("\t" + "helper functions: \t\t" + relFileName.get)      
      
      val contentSrcFileToRelLink = "srcFileToRelLink" + "=" + htmlOutputContext.filenamesOriginalToRelOutput.map(e=>(e._1.getAbsolutePath,e._2)).toJson.compactPrint
      
      // Generate output      
      val content = List(contentSrcFileToRelLink).mkString("\n\n")
      FileUtil.writeToFile(outputFilename,content)      

    } else {
      logger.error("Unable to generate js file with helper functions. No file name provided!")
    }
    
  }
  
  /**
   * Saves implicit information to disk in js file.
   * 
   * Included:
   *  - implicitConverions
   *  - implicitParameters
   * 
   * @param 
   */
  private def generateGlobalJSImplicitInfo(
      implicitConversions: Map[Int,ImplicitConversionInfoSummary],
      implicitParameters: Map[Int,ImplicitParamInfoSummary]) : Unit = {
    
    val relFileName = c.summaryFilesRelPath.get("js.global.implicitinfo")
    
    if(relFileName.isDefined){
      
      val outputFilename= new File(c.outputMainPath.getAbsolutePath + relFileName.get)
      logger.info("\t" + "implicit information: \t\t" + relFileName.get)      

      val implicitConv = "implicitConversions = " + implicitConversions.map(e=>(e._1.toString,e._2)).toJson.compactPrint
      val implicitPara = "implicitParameters = " + implicitParameters.map(e=>(e._1.toString,e._2)).toJson.compactPrint
   
      val content = List(implicitConv,implicitPara).mkString("\n\n")
      FileUtil.writeToFile(outputFilename,content)      

    } else {
      logger.error("Unable to generate js file with type information. No file name provided!")
    }
    
  }
  
  /**
   * Sets up the output folders and copies resources.
   * 
   */
  private def setupOutputEnvironment() : Unit = {
    val setter = new OutputContextSetter(c.outputMainPath)
    setter.setFolderStructure(c.outputRelFolders)
    c.resourcesToCopy.foreach({ f => setter.copyResource(f.base, new File(c.outputMainPath,f.target)) }) 
  }
  
   /**
   * Generates the output for the individual source files.
   * @param projectInfo ProjectInfo 
   */
  private def generateIndividualPages(projectInfo: ProjectInfo, htmlOutputContext: HtmlOutputContext) : Unit = {
    logger.info("Generating output for the source files ...")  
    
    val files = projectInfo.enrichedTokens.map(e => e._1).toList
    
    implicit val htmlOutputContext = new HtmlOutputContext(c.outputMainPath,files)
    
    
    projectInfo.enrichedTokens.foreach( e => {
      generateSrcFilePage(e._1,e._2,projectInfo.summary,htmlOutputContext)
    })
    
  }
  
  
  /**
   * Generate the output file for an individual source file.
   * @param srcFile    Source file
   * @param tokens  Enriched tokens of the source file.
   * @   TODO
   */
  private def generateSrcFilePage(
      srcFile: File, 
      tokens: ArrayBuffer[Token],
      projectSummary: ProjectSummary,
      htmlOutputContext: HtmlOutputContext) : Unit = {
      
      val srcFileLabel = htmlOutputContext.filenamesOriginalToShortened(srcFile)
      logger.info("\t" + srcFileLabel)

      // Get package name for file
      val packageName = projectSummary(ScalaLang.packageNamePerFile) match {
        case Some(packagePerFile) => {
          packagePerFile.get(srcFile).getOrElse("")
        }
        case None => { "" }
      }
     
      // Generate output       
      implicit val htmlSrcFileContext = new HtmlSrcFileContext(
          srcFile.getAbsolutePath,
          packageName,
          new TokenToOutputEntryHtml(htmlOutputContext))
      val srcEntries = generateHtmlEntries(tokens).toArray
    
      val outputFile = htmlOutputContext.filenamesOriginalToOutput(srcFile.getAbsolutePath())
      
      FileUtil.writeToFile(outputFile,htmlSrcFileContext.getBegin() + srcEntries.mkString("") + htmlSrcFileContext.getEnd())    

  }

  private def generateHtmlEntries(
      tokens: ArrayBuffer[Token])(implicit htmlContext: HtmlSrcFileContext)
    : Iterable[String] = {
 
    
    
    tokens.map(e=> e.text)
    
    // Group entries into: List[List[Token]] where each List contains a line of src code or a MultilineComment
    
    var idx_toProcess_Beg = 0;
    var idx_toProcess_End = 0;
    var currentLine = 0
    var codeTableOpen = false   // true: a code table environment is open
    var codeTableClose = false  // true: close code table environment
    val htmlEntries = scala.collection.mutable.ArrayBuffer[String]()

    // TODO: check again
    // 1 Token file? Check if loop works for this environment!!!
    // 
    while(idx_toProcess_End<(tokens.length-1)){
      
      // Find section to process next
      idx_toProcess_End = determineGroupOfTokensToBeProcessedNext(tokens,idx_toProcess_End)
     
      // Update codeTableClose?  
      codeTableClose = updateCodeTableClose(tokens,idx_toProcess_End)  
      
      // Process subsection of tokens
      val toProcess = tokens.slice(idx_toProcess_Beg, idx_toProcess_End).toArray
      //print("\n------------------\n" + toProcess.map(t=>t.text).mkString(";") )
     
      val (entries,currentLineUpdate,codeTableOpenUpdate) = processGroupsOfTokens(toProcess, currentLine, codeTableOpen, codeTableClose)
      
                 
      htmlEntries+= entries.mkString("\n")
      
      currentLine = currentLineUpdate
      codeTableOpen=codeTableOpenUpdate
      idx_toProcess_Beg = idx_toProcess_End  
    }
    
    // htmlEntries.foreach(t=>println(t))
    
    htmlEntries
    
    
  
    
  }
  
  /**
   * Determines a group of tokens to be processed together.
   * @param tokens                      Tokens.
   * @param idxLastTokenToProcessedNow  Last used index.
   * @return                            New index.
   */
  private def determineGroupOfTokensToBeProcessedNext(
      tokens: ArrayBuffer[Token],
      idxLastTokenToProcessedNow: Int) : Int = {
    var idx = idxLastTokenToProcessedNow
    do { idx += 1 }  while(
        idx<tokens.length &&
        tokens(idx)(ScalaLang.tokenType).isDefined && 
        tokens(idx)(ScalaLang.tokenType).get != ScalaLang.Tokens.MULTILINE_COMMENT && 
        !tokens(idx).text.contains("\n"))
      idx
  }
  
  /**
   * Processes group of tokens and returns their html output,
   * the line update and an indicator for an open code table.
   * @param toProcess       Tokens to process.
   * @param currentLine     Current line number.
   * @param codeTableOpen   Indicator true if code table open.
   * @param codeTableClose  Indicator true if code table has to be closed.
   * @return                Html entries, line number update, code table open indicator.
   */
  private def processGroupsOfTokens(
      toProcess: Array[Token],
      currentLine: Int,
      codeTableOpen: Boolean,
      codeTableClose: Boolean
      )(implicit htmlContext: HtmlSrcFileContext) : (Array[String],Int,Boolean) = {
    
    
    
    var currentLineUpdate = currentLine
    var codeTableOpenUpdate = codeTableOpen
      
    // DEBUG
    // Print the text from the grouped tokens
    // println(toProcess.map(e=>e.text).mkString(";") +"\n-------------------------------------------------------")
    
    // IDEA:     return WS Strig as indentations for the next section!!! 
    // PROBLEM:  Issue that WS with \n ends Group to process but has WS for next line indentation!!
    
    // TEXT ENTRY
    val entries = if(toTextEntry(toProcess)){
    
      // Close code table and update variables
      codeTableOpenUpdate = false
      
      currentLineUpdate += toProcess(0).text.count(_ == '\n') + 1
      
      // Text
      val (wrap_beg,wrap_end) = htmlContext.tokenToOutputEntry.getTokenEntryWrapper(toProcess(0))
      
      
      if(codeTableOpen){
        Array(htmlContext.codeTable_getEnd() + 
            htmlContext.textTable_getBegin() + 
            htmlContext.textTable_getEntry("", wrap_beg+ wrap_end) + 
            htmlContext.textTable_getEnd())
      } else {
        Array(htmlContext.textTable_getBegin() + 
        htmlContext.textTable_getEntry("", wrap_beg+ wrap_end) + 
        htmlContext.textTable_getEnd())
      }
      
    // CODE ENTRY
    } else {
     
      // Determine code section surrounding
      val (table_beg,table_end) = if(!codeTableOpen && !codeTableClose){
        codeTableOpenUpdate = true
        (htmlContext.codeTable_getBegin(),"")
      } else if (!codeTableOpen && codeTableClose){
        codeTableOpenUpdate = false
        (htmlContext.codeTable_getBegin(), htmlContext.codeTable_getEnd())
      } else if (codeTableOpen && !codeTableClose){
        codeTableOpenUpdate = true
        ("","")
      } else {
        codeTableOpenUpdate = false
        ("",htmlContext.codeTable_getEnd())
      }
      
      // Get html token wrapper
      val wrapper = toProcess.map(t=>{
        htmlContext.tokenToOutputEntry.getTokenEntryWrapper(t)
      })
      
           
      // Determine (a) all tokens in one line (b) parts of tokens to be split over lines
      val totalLineUpdate = toProcess.map(t=>t.text).mkString("").count(_ == '\n')
      
      val tmp = ArrayBuffer[String]()
      var str = ""
      
      // Process tokens
      for(i <- 0 until toProcess.length){
        val t = toProcess(i)
        val (beg,end) = wrapper(i)
            
        // Token text with '\n'
        if(t.text.contains('\n')){
          
          // WS token          
          if(t(ScalaLang.tokenType).isDefined && t(ScalaLang.tokenType).get == ScalaLang.Tokens.WS){
               
            val numNewLines = t.text.count(_ == '\n')
            val idx_FirstNewLine = t.text.indexOf("\n")
            val idx_LastNewLine = t.text.lastIndexOf("\n")
            //str = str + (beg + t.text.slice(0,idx_FirstNewLine) + "[a]" + end)
            
            //println("WS" + t.offset + " - NL: " + numNewLines + " WSL: " + t.length)
            
            //tmp.append(str)
            str = ""
            
            if(numNewLines>2){
              for(k<- 3 to numNewLines){
                tmp.append("")
              }
            } 
            
            if(idx_LastNewLine < (t.text.length-1)){
              str = (beg + t.text.slice(idx_LastNewLine+1,t.text.length) + end)
            }
          // Token w/ \n, but not WS  
          } else {
            val splitted = t.text.split('\n')
            
            val numNewLines = t.text.count(_ == '\n')
            val idx_FirstNewLine = t.text.indexOf("\n")
            val idx_LastNewLine = t.text.lastIndexOf("\n")
            
            for (k <- 0 until (splitted.length-1)){
              str = str + (beg + splitted(k) + end)
              tmp.append(str) 
              str = ""
            }
            
            str = (beg + splitted.last + end)
            if(idx_LastNewLine==t.text.length-1){
              tmp.append(str)
              str=""
            }
          }
          
        // Tokens w/o '\n'  
        } else {
         
          // Escape xml/html in xml and literals
          val escapedText = if(t(ScalaLang.tokenType).isDefined){
            val tType = t(ScalaLang.tokenType).get
            // Handles also Integer_Literal,Float_Literal,true,false,null, but with no effect.
            if(tType.isXml || tType.isLiteral){
              import org.codeprose.consumer.util.XmlEscape
              XmlEscape.escape(t.text)
            } else {
              t.text
            }
            
          } else { t.text }
          
          str = str + (beg + escapedText + end)
        }
        
      }
      
      if(str.length!=0){
        tmp.append(str)
      }
      
      //tmp.foreach(e => print(e))
      val rawEntries = tmp.map( l => {
        val o = htmlContext.codeTable_getEntry("",currentLineUpdate, l)
        currentLineUpdate+=1
        o
      }).toArray
      
            
      val packagedEntries = Array(table_beg) ++ rawEntries ++ Array(table_end)
//      packagedEntries.foreach(e=>println(e))
      packagedEntries
    }
    
    
    //entries.foreach(x=>print(x))
    
    (entries,currentLineUpdate,codeTableOpenUpdate)
    
  }
  
  
  /**
   * Returns indicator whether code table needs to be closed.
   * @param tokens                      All tokens of a file.
   * @param idxLastTokenToProcessedNow  Index of last token in current group to be processed.
   * @return                            Boolean indicator. 
   */
  private def updateCodeTableClose(tokens: ArrayBuffer[Token], idxLastTokenToProcessedNow: Int) : Boolean = {

	  val lastIdx = tokens.length-1

			  if(idxLastTokenToProcessedNow >= lastIdx){
				  true  
			  } else {
				  val nextIdx = idxLastTokenToProcessedNow+1
						  val nextToken = tokens(nextIdx)
						  val nextTokenType = nextToken(ScalaLang.tokenType)

						  if (nextTokenType.isDefined && 
								  nextTokenType.get == ScalaLang.Tokens.MULTILINE_COMMENT &&
								  !CommentUtil.isScalaDocComment(nextToken.text)){ 
							  true
						  } else {
							  false
						  }
			  }
  }

  /**
   * Checks if tokens have to processed as text.
   * @param toProcess Tokens to process.
   * @return          Boolean indicator. 
   */
  private def toTextEntry(toProcess: Array[Token]) : Boolean = {
   if(toProcess.length==1 && 
       toProcess(0)(ScalaLang.tokenType).isDefined &&
       toProcess(0)(ScalaLang.tokenType).get == ScalaLang.Tokens.MULTILINE_COMMENT && 
       !CommentUtil.isScalaDocComment(toProcess(0).text)
       ){
     true
   } else { 
     false 
   }
  }  
  
}




