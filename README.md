# codeprose

**Leveraging Editor Services for Literate Programming**

## What is codeprose?

Codeprose is a literate programming tool for Scala. It generates HTML output including 
syntax highlighting and source code annotated with compile time information. 
 
Tokens are enriched with detailed type information, links to declaration and 
type definition as well as information about implicit conversions and parameters. 
 
Further, both type and package summaries as well as 'where used' information is provided.
  
In the spirit of literate programming Codeprose allows syntax formatting via Markdown 
in comments. Compile time information is obtained by interfacing with the editor 
service [ENSIME](https://github.com/ensime/) which guarantees independence from the Scala compiler and improves 
re-usability and maintenance.
  
Codeprose's design makes it easily extendible to other or mixtures of languages and 
offers the possibility of introducing other output formats such as, for example, LaTeX.


## Output examples:

[codeproseTestProject](https://github.com/gushai/codeproseTestProject) contains a test project. The generated output is located in folder ```output```.


## Comment

- Codeprose is still a beta! The current version is build against a fork of the [ensime-server](https://github.com/ensime/ensime-server) that can be found at [gushai/ensime-server](https://github.com/gushai/ensime-server). This repository also includes the ensime-client that is used by codeprose. The ensime-client needs to be published locally.

- There is a new and much improved [ensime-server](https://github.com/ensime/ensime-server) available. Codeprose is in desperate need for an update to make full use of the improvements in ENSIME's latest version.

## Installation

#### Prerequisites

-    [sbt](http://github.com/sbt/sbt) (to run the ensime-server and to generate the ensime files)
-    [ensime-sbt](https://github.com/ensime/ensime-sbt) (to generate the ensime file)
-    ensime-server and ensime-client from [gushai/ensime-server](https://github.com/gushai/ensime-server)
-    [codeprose](https://github.com/gushai/codeprose)
-    Project to run codeprose on. (See e.g. [gushai/codeproseTestProject](https://github.com/gushai/codeproseTestProject))

#### 1. Get ensime-server and ensime-client.

1.   Clone the fork of the [ensime-server](https://github.com/gushai/ensime-server.git) (includes the ensime-client) at [gushai/ensime-server](https://github.com/gushai/ensime-server) to a local folder. Please use the branch "client_basedon_20150730".
2.   Start sbt in the ensime-server folder and run `publishLocal` to publish the ensime-client codeprose uses.

#### 2. Get codeprose

Clone [codeprose](https://github.com/gushai/codeprose.git) at [gushai/codeprose](https://github.com/gushai/codeprose) to a local folder.

#### 3. Setup a project to run codeprose on

To run codeprose the ensime environment needs to be set up in the project. Check out the test projects at  [gushai/codeproseTestProject](https://github.com/gushai/codeproseTestProject) as an example.


 1.   Add  [ensime-sbt](https://github.com/ensime/ensime-sbt)  to your projects plugins by adding 

    ```scala
    addSbtPlugin("org.ensime" % "ensime-sbt" % "0.1.7")
    ```
    either to  `project/plugins.sbt` or `~/.sbt/0.13/plugins/plugins.sbt` if you want to use ensime with every project.

 2.   Create the folder `yourProject/.ensime_cache/` to allow ensime to store its information.
 3.   Start sbt in your project and run  `gen-ensime` to generate the `.ensime` file.


## Run codeprose

#### 1. Start ensime-server

Go into the ensime-server folder from installation step 1.1 and execute 

`sbt '; project server ; set javaOptions+="-Densime.config=/path/to/your/project/.ensime"; set javaOptions+="-Densime.protocol=jerk" ; run' `

to start the server.

**Note**: If you start the ensime-server for the first time on a project it takes a while until the ensime-server has  gathered all information. Be patient.

#### 2. Run codeprose

Go into the codeprose folder from installation step 2, start sbt and execute the following command

`run --sbtProject "/path/to/your/Project/" --outputFolder "/path/to/output/folder/"`



## Known issues:

-    Running codeprose with a newly started server may cause some SymbolInfoReq to time out. Run codeprose again if you are not satisfied with the output.
-   Some ensime responses can not be parsed by spray.json in the ensime-client (implemented in the ensime-server). Those requests kill the underlying network socket. All further requests time out.



