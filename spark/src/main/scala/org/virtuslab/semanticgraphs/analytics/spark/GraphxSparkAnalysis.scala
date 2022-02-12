package org.virtuslab.semanticgraphs.analytics.spark
import org.apache.spark.*
import org.apache.spark.graphx.*
import org.apache.spark.graphx.lib.ShortestPaths
import org.apache.spark.sql.SparkSession
import org.virtuslab.semanticgraphs.analytics.scg.SemanticCodeGraph
import org.virtuslab.semanticgraphs.analytics.spark.KBetweenness
import org.virtuslab.semanticgraphs.analytics.{scg, utils}
// To make some of the examples work we will also need RDD
import org.apache.spark.rdd.RDD

object GraphxSparkAnalysis extends App:

  val projectToAnalyse = SemanticCodeGraph.commonsIO
  // we need to translate string ids to Long ids for spark graphx API
  var counter = 0L
  val stringToIntMap = scala.collection.mutable.Map.empty[String, Long]
  val rawNodes =
    scg.SemanticCodeGraph.read(projectToAnalyse).withoutZeroDegreeNodes().nodesMap.toSeq.map { case (key, node) =>
      val id = counter
      stringToIntMap.update(key, id)
      counter += 1
      (id, node)
    }
  val rawEdges = rawNodes.flatMap { case (id, node) => node.edges.map(e => Edge(id, stringToIntMap(e.to), e)) }

  val spark = SparkSession
    .builder()
    .master("local[1]")
    .appName("SparkByExamples.com")
    .getOrCreate()

  val sc = spark.sparkContext
  val nodes = sc.parallelize(rawNodes)
  val edges = sc.parallelize(rawEdges)

  val graph = Graph(nodes, edges)

  val kbetweenness = KBetweenness.run(graph, 10)
  // println(graph.joinVertices(kbetweenness.vertices)((v, node, b) => node).numVertices)
  ShortestPaths
    .run(graph, stringToIntMap.values.toSeq)
    .mapVertices((v, m) => m.map { case (v2, value) => println(s"$v -$value-> $v2") })

  println(s"GraphSize: ${graph.numVertices}, edges: ${graph.numEdges}")

  sc.stop()
