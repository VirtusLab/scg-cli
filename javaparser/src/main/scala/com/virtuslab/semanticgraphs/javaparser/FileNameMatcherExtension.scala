package com.virtuslab.semanticgraphs.javaparser

import org.eclipse.jgit.fnmatch.FileNameMatcher

extension (matcher: FileNameMatcher) {

  def matches(str: String): Boolean = {
    matcher.append(str)
    val result = matcher.isMatch
    matcher.reset()
    result
  }

}
