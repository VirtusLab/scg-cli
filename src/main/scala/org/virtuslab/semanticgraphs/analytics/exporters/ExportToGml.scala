package org.virtuslab.semanticgraphs.analytics.exporters

import org.jgrapht.nio.{Attribute, AttributeType, DefaultAttribute}
import org.virtuslab.semanticgraphs.analytics.scg.SemanticCodeGraph

import java.io.File
import org.jgrapht.nio.graphml.GraphMLExporter
import org.jgrapht.nio.graphml.GraphMLExporter.AttributeCategory
import org.virtuslab.semanticgraphs.analytics.scg.ScgJGraphT.{exportUndirected, LabeledEdge}

import scala.jdk.CollectionConverters.MapHasAsJava

object ExportToGml {

  def exportToGML(
    outputFileName: String,
    semanticCodeGraph: SemanticCodeGraph,
    nodeAttributes: Map[String, Map[String, Attribute]] = Map.empty.withDefaultValue(Map.empty)
  ): Unit =
    val f = new File(outputFileName)
    val exporter = new GraphMLExporter[String, LabeledEdge]()
    exporter.setVertexLabelAttributeName("label")

    val toRegister = nodeAttributes.values.flatMap { map =>
      map.map { case (key, value) => (key, value.getType) }
    }.toSet

    toRegister.foreach { case (key, attributeType) =>
      exporter.registerAttribute(key, AttributeCategory.NODE, attributeType)
    }

    exporter.setExportVertexLabels(true)
    exporter.setVertexAttributeProvider { id =>
      semanticCodeGraph.nodesMap.get(id) match {
        case Some(node) =>
          (Map("label" -> new DefaultAttribute(node.displayName, AttributeType.STRING)) ++
            nodeAttributes.getOrElse(
              id,
              Map.empty
            )).asJava
        case None =>
          nodeAttributes
            .getOrElse(
              id,
              Map.empty
            )
            .asJava
      }
    }

    exporter.exportGraph(semanticCodeGraph.graph, f)

}
