package org.codeprose.util

import org.scalatest.FunSpec


class StringUtilSpec extends FunSpec {

  
  describe("StringUtil.findLongtestCommonPrefix()"){
  
    import org.codeprose.util.StringUtil
    
    it("should return an empty string for empty input:"){
        assert(StringUtil.findLongtestCommonPrefix(List[String]()).equals(""))
     }
    
    it("should return an empty string for inputs with no common prefix (2 inputs):"){    
        assert(StringUtil.findLongtestCommonPrefix(List("asdf","1234")).equals(""))
     }   
    
    it("should return an empty string for inputs with no common prefix (3 inputs):"){    
        assert(StringUtil.findLongtestCommonPrefix(List("asdf","1234","ASFD")).equals(""))
     }
    
    it("should return correct prefix"){    
        assert(StringUtil.findLongtestCommonPrefix(List("asdf","as1211","asDF1")).equals("as"))
    }
    
    it("should return prefix with input all the same"){    
        assert(StringUtil.findLongtestCommonPrefix(List("asdf","asdf","asdf")).equals("asdf"))
    }
    
  }
  
  
  describe("StringUtil.getUniqueShortFileNames()"){
    
    import org.codeprose.util.StringUtil
    
    it("should return an empty string for empty input:"){    	
      val filenames = StringUtil.getUniqueShortFileNames(List[String]())
      assert(filenames.size == 0)
    }

    it("should return the input for one string supplied:"){
      val input = List[String]("/path/to/file.scala")
      val filenames = StringUtil.getUniqueShortFileNames(input)
      assert(filenames.mkString("\n") == input.mkString("\n"))
    }
    
    it("should return the input for inputs with no common prefix (2 inputs):"){
      val input = List("/one/one.file","/two/two.file")
      val check = List("/one/one.file","/two/two.file")
    	val filenames = StringUtil.getUniqueShortFileNames(input)      
      assert(filenames.mkString("\n") == check.mkString("\n"))      
    }    
    
    it("should return a list of files that are in a common directory"){
    	val input = List("/one/two/file.one","/one/two/file.two","/one/two/file.three")
      val check = List("/file.one","/file.two","/file.three").mkString("\n")
      val filenames = StringUtil.getUniqueShortFileNames(input)
      
      assert(filenames.mkString("\n") == check)
    }
    
     
    it("should return a list of files that are in a common directory but share prefix in last directory"){
      val input = List(
          "/one/two/a.scala",
          "/one/two/b.scala",
          "/one/two/c.scala",
          "/one/three/f1.scala",
          "/one/three/f2.scala",
          "/one/three/f3.scala")
      
          // TODO: Issue: getUniqueShortFileNames needs to be adjusted to only remove prefixes that match a full directory path.
          // I.e. /one/two/* and /one/three/* should result in only the /one/ being removed!
          
      val check = List(
          "/two/a.scala",
          "/two/b.scala",
          "/two/c.scala",
          "/three/f1.scala",
          "/three/f2.scala",
          "/three/f3.scala"
          ).mkString("\n")
      val filenames = StringUtil.getUniqueShortFileNames(input)
      
      assert(filenames.mkString("\n") == check)
    }
  }
    

    describe("StringUtil.getUniqueShortFileNamesGroupedByDirectory()"){
    
    import org.codeprose.util.StringUtil
    
    it("should return an empty string for empty input:"){     
      val filenames = StringUtil.getUniqueShortFileNamesGroupedByDirectory(List[String]())
      assert(filenames.size == 0)
    }

    it("should work for single absolute path supplied:"){
      
      val input = List[String]("/path/to/file.scala")
      val filenames = StringUtil.getUniqueShortFileNamesGroupedByDirectory(input)
      assert(filenames("/path/to").head == (0,"file.scala"))          
    }

    it("should group properly with all files sharing a commong path"){
      val input = List(
          "/one/two/a.scala",
          "/one/two/b.scala",
          "/one/two/c.scala",
          "/one/three/f1.scala",
          "/one/three/f2.scala",
          "/one/three/f3.scala")

      val filenames = StringUtil.getUniqueShortFileNamesGroupedByDirectory(input)
      assert(filenames("/two").map(e => e._2).mkString(" ") == "a.scala b.scala c.scala")
      assert(filenames("/two").map(e => e._1).mkString(" ") == "3 4 5") // Input is sorted!
      
      assert(filenames("/three").map(e => e._2).mkString(" ") == "f1.scala f2.scala f3.scala")
      assert(filenames("/three").map(e => e._1).mkString(" ") == "0 1 2")
                
    }
    
    it("should group properly without all files sharing a commong path"){
      val input = List(
          "/m/two/a.scala",
          "/m/two/b.scala",
          "/m/three/f1.scala",
          "/m/three/f2.scala",
          "/t/two/a.scala",
          "/t/two/b.scala",
          "/t/three/f1.scala",
          "/t/three/f2.scala"   
      )

      val filenames = StringUtil.getUniqueShortFileNamesGroupedByDirectory(input)
      assert(filenames("/m/two").map(e => e._2).mkString(" ") == "a.scala b.scala")
      assert(filenames("/m/three").map(e => e._2).mkString(" ") == "f1.scala f2.scala")
      assert(filenames("/t/two").map(e => e._2).mkString(" ") == "a.scala b.scala")
      assert(filenames("/t/three").map(e => e._2).mkString(" ") == "f1.scala f2.scala")
          
    }
    
  }
  
    
}