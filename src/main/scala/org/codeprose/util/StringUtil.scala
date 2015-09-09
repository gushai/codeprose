package org.codeprose.util

object StringUtil {

  /**
   * Finds the longest common prefix in a list of strings.
   * @param   s List of strings.
   * @return    Longest common prefix. If s.length=0 return "".
   */
  def findLongtestCommonPrefix(s: List[String]) : String = {
     if(s.isEmpty)
       return ""
     else if (s.length==1){
       return s(0)
     }
     else {        
       val shortestStringLength = s.map(s=>s.length).foldLeft(Int.MaxValue){(min,a)=>Math.min(min, a)}      
       var k=0
       var matches = true
       while(k < shortestStringLength && matches ){         
         var i=0
         while(i< (s.length-1) && matches){
           if(s(i).charAt(k) != s(i+1).charAt(k)){
             matches = false              
           }
           i+=1
         }
         if(matches)
           k+=1
       }
     
       if(k>0)
         return s(0).take(k)
       else
         return ""
     }
  }
  /**
   * Removes the longest common path of all elements.
   * @param   s List of String
   * @return    List of Strings with the common prefix removed.
   */
  def getUniqueShortFileNames(s: List[String]) : List[String] = {      
    if(s.size<=1){return s }
    val prefixRaw = findLongtestCommonPrefix(s)
    val prefixLength = Math.min(prefixRaw.length,prefixRaw.lastIndexOf("/")) 
    val sliceFirstIndex = List.fill(s.length)(prefixLength)  
    (sliceFirstIndex zip s).map(e => (e._2.slice(e._1,e._2.length)))
  }
    
  /**
   * Groups files by path after removing the most common path.
   * 
   * NOT FINISHED!
   * 
   * @param   s List of filenames.
   * @return    Filenames group by common prefix with position in incoming file list.
   */
  def getUniqueShortFileNamesGroupedByDirectory(s: List[String]) : Map[String,List[(Int,String)]] = {      
    if(s.size==0) { 
      return Map[String,List[(Int,String)]]()
    } 
    val files = s.toArray
    scala.util.Sorting.quickSort(files)
    val shortened = getUniqueShortFileNames(files.toList)
    val shortenedWithId = shortened.zipWithIndex
    shortenedWithId.map(e=> {
      val idx = e._1.lastIndexOf("/")
     (e._1.slice(0,idx),(e._2,e._1.slice(idx+1,e._1.length)))
    }).groupBy(e => e._1).mapValues(e => e.map(e => (e._2._1,e._2._2)))
    
//    shortened.map(e => {
//     val idx = e.lastIndexOf("/")
//     (e.slice(0,idx),e.slice(idx+1,e.length))
//    }).groupBy(e => e._1).mapValues(e => e.map(e => e._2))    
  }
  
  
  
  
}