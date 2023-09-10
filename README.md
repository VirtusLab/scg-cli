# Semantic Code Graph analytics

## About

The `scg-cli` tool was built to augment and speed up software comprehension process for analysing and discovering project software architecture, structure and dependencies.

The `scg-cli` tool extracts semantic information about code structure and dependencies from the Java and Scala projects, 
and structures it as a Semantic Code Graph, an information model underlying `scg-cli`. 
The information is written into a portable, open protobuf-based [format](https://github.com/VirtusLab/graphbuddy/blob/master/proto/graph_node.proto). 
Based on the extracted information, the `scg-cli` command line tool provides project overview, finds the most critical code entities, and computes project partitioning. 
The results of this analysis and the SCG data can be exported for further investigation by external tools such as Gephi software (visualization) and, notably, 
as a Jupyter Notebook environment with helper APIs to enable advanced analysis of the project using data analytics methods.

## Requirements

The `scg-cli` tool is a Scala written software and for execution you need Java Runtime Environment (at least 1.8 version) configured on your environment.
For some additional features `Docker` software and available `docker` command is required (e.g. exporting software dependency graph for further investigation in Jupyter notebook).

## Downloading `scg-cli`

Ready to use `scg-cli` binaries in `*.zip` file are published as GitHub releases, see [here](https://github.com/VirtusLab/scg-cli/releases).
After downloading and unzipping you can find `scg-cli` executable script in `bin` folder.

## Generating project SCG metadata

## Java
For Java use `scg-cli generate /path/to/project` command. This will create protobuf data in `.semanticgraphs` folder which will be later consumed by other `scg-cli` functionalities.

## Scala
For Scala, add to the project [Scala Compile Plugin](https://github.com/liosedhel/semantic-code-graph-scala2). For Scala sbt project, use sbt plugin:
```
addSbtPlugin("org.virtuslab.semanticgraphs" % "sbt-plugin" % "0.2.19")
```
or use Scala Compiler Plugin directly
```
addCompilerPlugin("org.virtuslab.semanticgraphs" % "scalac-plugin" % "0.2.19" cross CrossVersion.full)
scalacOptions += "-Yrangepos"
```
And then recompile the project.

## Analyzing the project with `scg-cli`

For user convenience there is `scg-cli help` command with short explanation of the most important functionalities:

```text
$ scg-cli help
Usage: scg-cli [COMMAND]
CLI to analyse projects based on SCG data
Commands:
  help       Display help information about the specified command.
  crucial    Find crucial code entities.
  export     Export SCG metadata for further analysis
  generate   Generate SCG metadata.
  partition  Suggest project partitioning.
  summary    Summarize the project.
  version    Show scg-cli version.
```

For example, to print summary for your project in HTML format use:
```bash
$ scg-cli summary -o html path/to/project
```

### Using `scg-cli` project examples (already generated metadata) 

If you don't want to generate metadata for new project and still play with `scg-cli` features, there is `data` folder where you can find extracted and zipped `*.semanticgraphs` files for some popular Java and Scala projects. You can try to analyse them with:

```bash
$ scg-cli summary data/metals-0.10.3.zip 
```

### Class Collaboration Network and Call Graph

The `scg-cli` tool is built upon the Semantic Code Graph model (SCG). It allows you to generate two key components: the Class Collaboration Network (CCN) and the Call Graph (CG) from the SCG data.
As a result, most of the `scg-cli` commands include a `-g` switch with `SCG`, `CCN` or `CG` options, which enables you to analyze a specific subgraph instead of the entire `SCG`. For example:
```bash
$ scg-cli summary -g CCN data/metals-0.10.3.zip
```
This command will generate a summary for the Class Collaboration Network of the `metals` project.



## Building `scg-cli` manually

Download the `scg-cli` sources (clone the GitHub repo). Install [sbt](https://www.scala-sbt.org/download.html) Build the `scg-cli` with:
```bash
$ sbt stage
```
You will find executable binaries in `./target/universal/stage/bin/`.

## Building new release files

```bash
$ sbt universal:packageBin
```