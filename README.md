# Semantic Code Graph analytics

## Using scg-cli

Build the `scg-cli` with:
```bash
$ sbt stage
```

and follow the `scg-cli` help page:

```bash
$ ./target/universal/stage/bin/scg-cli help  
Usage: scg-cli [COMMAND]
CLI to analyse projects based on SCG data
Commands:
  help       Display help information about the specified command.
  crucial    Find crucial code entities
  generate   Generate SCG metadata
  partition  Suggest project partitioning.
  summary    Summarize the project
```

### Example 

In `data` folder you can find extracted and zipped `*.semanticgraphs` files. You can try to analyse them with:

```bash
$ ./target/universal/stage/bin/scg-cli summary data/metals.zip 
```

## Using on your java project

Generate Semantic Code Graph information model metadata:
```bash
$ ./target/universal/stage/bin/scg-cli generate path/to/project
```

Start to analyse your project, e.g.:
```bash
$ ./target/universal/stage/bin/scg-cli summary path/to/project
```


## Building new release

```bash
$ sbt universal:packageBin
```