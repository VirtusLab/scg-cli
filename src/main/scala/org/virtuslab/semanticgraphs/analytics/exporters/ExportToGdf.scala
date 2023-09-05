package org.virtuslab.semanticgraphs.analytics.exporters

import com.virtuslab.semanticgraphs.proto.model.graphnode.Location
import org.virtuslab.semanticgraphs.analytics.scg.SemanticCodeGraph
import org.virtuslab.semanticgraphs.analytics.utils.*

import java.io.File

object ExportToGdf:

  /**
    * Export graph to standard .GDF format, see: https://gephi.org/users/supported-graph-formats/gdf-format/
    */
  def exportToGdf(
    outputFileName: String,
    semanticCodeGraph: SemanticCodeGraph
  ): Unit =
    // val scg = semanticCodeGraph.withoutZeroDegreeNodes()
    val scg = semanticCodeGraph
    val f = new File(outputFileName)
    val printer = new java.io.PrintWriter(f)

    printer.println(
      "nodedef> name VARCHAR, label VARCHAR, kind VARCHAR, uri VARCHAR, location VARCHAR, loc INTEGER"
    )
    // format: off
    scg.nodes.foreach { node =>
      import node._
      printer.println(
        s"${cleanId(id)}, $displayName, $kind, ${location.map(_.uri).getOrElse("")}, ${location.map(printLocation).getOrElse("")}, ${properties.get("LOC").map(_.toInt).getOrElse(0)}"
      )
    }
    printer.println(
      "edgedef> source VARCHAR, target VARCHAR, type VARCHAR, directed BOOLEAN, uri VARCHAR, location VARCHAR, label VARCHAR"
    )
    scg.nodes.foreach { node =>
      node.edges.foreach { edge =>
        import edge._
        printer.println(
          s"${cleanId(node.id)}, ${cleanId(to)}, ${`type`}, true, ${location.map(_.uri).getOrElse("")}, ${location.map(printLocation).getOrElse("")}, ${`type`}"
        )
      }
    }
    // format: on

    def cleanId(id: String): String =
      id.replace(" ", "_").replace(",", "-")

    def printLocation(location: Location): String = {
      import location._
      s"$startLine:$startCharacter;$endLine:$endCharacter"
    }
    printer.close()
