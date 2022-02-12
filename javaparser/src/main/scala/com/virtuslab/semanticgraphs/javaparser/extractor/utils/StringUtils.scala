package com.virtuslab.semanticgraphs.javaparser.extractor.utils

extension (s: String) {
  def stripPackagePrefix: String = s.split("\\.").last
}
