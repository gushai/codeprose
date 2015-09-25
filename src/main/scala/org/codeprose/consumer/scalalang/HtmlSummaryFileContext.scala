package org.codeprose.consumer.scalalang

/**
 * Provides the HTML frame for a summary page (incl. index.html).
 * 
 * Includes 
 *  - HTML head (incl. scripts)
 *  - header and footer
 */
class HtmlSummaryFileContext(){

  def htmlDataAttributePrefix  = "cp-"  
    
  /**
   * Returns the upper part of an output html file. This includes:
	 *  - html headers
	 *  - js scripts (per token and per File)
	 *  - header div
	 * @return    String containing the above information.  
	 */
  def getBegin(title: String,script: String,withLinkToOverview: Boolean) : String = {
    
    val linkToOverview = if(withLinkToOverview){
      s"""<a style="color:black;font-weight:bold;" href="./index.html">overview</a>"""
    } else {
      s""""""
    }
    
  s"""
<!doctype HTML>
<html lang="en">
<head>
<meta http-equiv="content-type" content="text/html; charset=utf-8" />
<link rel="stylesheet" type="text/css" href="./style/style.css" media="screen" />
<title>$title</title>
<script src="./js/codeprose.global.js"></script>
<script src="https://code.jquery.com/jquery-1.10.2.js"></script>
<script src="http://ajax.aspnetcdn.com/ajax/jquery.ui/1.10.0/jquery-ui.js"></script>
<script src="./js/codeprose.typeinformation.js"></script>
<script src="./js/codeprose.whereusedinformation.js"></script> 
<script src="./js/codeprose.packageinformation.js"></script>
<script src="./js/codeprose.helper.js"></script>
<script src="./js/codeprose.implicitinformation.js"></script>  

<script type="text/javascript">

$script      

</script>
</head>
<body>
<div align="center">
<div class="header" id="header" align="center"> 
<div style="float:left;"> 
<span style="font-size:1.5em;font-weight:bold;">$title&nbsp;</span></div>
<div style="float:right;">$linkToOverview</div>
</div>
<div class="content">
<noscript><div style="margin:2em;"><b>Activate JavaScript for more features</b>.</div></noscript>\n
"""
  }
  /**
   * Return the lower part of the html output. This includes:
   *  - html footer 
   * @return  String containing the lower part of the html output.
   */
  def getEnd() : String  = {
    s"""\n\n<div class="footer">generated by codeprose. help and support on <a href="http://github.com/gushai/codeprose" target="blank">github</a>.</div>\n</div>\n</div>\n</body>\n</html>"""
  }
  
  /**
   * Provides a div to wrap summary page content in.
   * 
   * Use for "summary" and "details" in summary pages.
   * 
   * @param title   Section title
   * @param content HTML content to be places inside div.
   */
  def packageContent(title: String, content: String) : String = {
	  
    val frameBeg = s"""<div class="textframe">\n<h2>$title</h2>\n<div class="textbox">\n"""
	  
    val frameEnd = s"""\n</div>\n</div>\n\n"""
    
    frameBeg + content + frameEnd

  }

}





