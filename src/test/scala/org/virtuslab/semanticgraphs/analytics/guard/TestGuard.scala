package org.virtuslab.semanticgraphs.analytics.guard

class TestGuard {

  def testGuard(): Unit = {
    val archGuard = CheckArchitecture.readNodes("data")

    archGuard.methods().inPackage("domain").methods.values.flatMap(_.edges).foreach(println)
    archGuard.methods().inPackage("domain").shouldNotCall(node => node.properties.get("package").forall(_.contains("infrastructure")))
  }

}
