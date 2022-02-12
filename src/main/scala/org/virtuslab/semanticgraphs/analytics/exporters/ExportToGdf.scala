package org.virtuslab.semanticgraphs.analytics.exporters

import org.virtuslab.semanticgraphs.analytics.scg.SemanticCodeGraph
import org.virtuslab.semanticgraphs.analytics.utils._

import java.io.File

object ExportToGdf:

  /**
    * Export graph to standard .GDF format, see: https://gephi.org/users/supported-graph-formats/gdf-format/
    */
  def exportToGdf(
    outputFileName: String,
    semanticCodeGraph: SemanticCodeGraph
  ): Unit =
    val f = new File(outputFileName)
    val printer = new java.io.PrintWriter(f)

    printer.println(
      "nodedef> name VARCHAR, label VARCHAR, kind VARCHAR, uri VARCHAR, loc INTEGER"
    )
    semanticCodeGraph.nodes.foreach { node =>
      import node._
      printer.println(
        s"${id.replace(" ", "_")}, $displayName, $kind, ${location
            .map(_.uri)
            .getOrElse("")}, ${properties.get("LOC").map(_.toInt).getOrElse(0)}"
      )
    }
    printer.println(
      "edgedef> source VARCHAR, target VARCHAR, type VARCHAR, directed BOOLEAN, uri VARCHAR, label VARCHAR"
    )
    semanticCodeGraph.nodes.foreach { node =>
      node.edges.filter(_.location.isDefined).foreach { edge =>
        import edge._
        printer.println(
          s"${node.id.replace(" ", "_")}, $to, ${`type`}, true, ${location.map(_.uri).getOrElse("")}, ${`type`}"
        )
      }
    }

    printer.close()
