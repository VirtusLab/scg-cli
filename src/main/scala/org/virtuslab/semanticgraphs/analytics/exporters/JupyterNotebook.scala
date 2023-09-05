package org.virtuslab.semanticgraphs.analytics.exporters

import org.virtuslab.semanticgraphs.analytics.partitions.DockerDistribution
import os.ProcessOutput

import java.io.File
import scala.util.Try

// -d -p 8888:8888 -p 4040:4040 -p 4041:4041
object JupyterNotebook:
  def runJupyterNotebook(workspace: String): Unit =
    val absoluteWorkspacePath = new File(workspace)
    println(absoluteWorkspacePath.getAbsolutePath + ":/home/jovyan/data")
    val containerName = "scg-jupyter"
    Try(os.proc("docker", "stop", containerName).call())
    Try(os.proc("docker", "rm", "--force", containerName).call())
    Thread.sleep(2000) // wait for previous docker jupyter shutdown
    os.proc(
      "docker",
      "run",
      "-d",
      "-p",
      "8888:8888",
      "-p",
      "4040:4040",
      "-v",
      absoluteWorkspacePath.getAbsolutePath + ":/home/jovyan/data",
      "--name=scg-jupyter",
      DockerDistribution.scgJupyterImage
    ).call()

    Thread.sleep(5000)
    println("SCG Jupyter Notebook available under:")
    val lines = os.proc("docker", "logs", "scg-jupyter").call(mergeErrIntoOut = true).out.lines()
    lines.filter(line => line.contains("http") && line.contains("token=")).foreach(println)

end JupyterNotebook

@main
def main() =
  println("SCG Jupyter Notebook available under:")
  val lines = os.proc("docker", "logs", "scg-jupyter").call(mergeErrIntoOut = true).out.lines()
  lines.filter(line => line.contains("http") && line.contains("token=")).foreach(println)
