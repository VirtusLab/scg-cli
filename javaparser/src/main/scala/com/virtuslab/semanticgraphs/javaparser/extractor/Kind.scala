package com.virtuslab.semanticgraphs.javaparser.extractor

enum NodeKind:
  case CLASS, CONSTRUCTOR, METHOD, INTERFACE, VARIABLE, PARAMETER, VALUE, TYPE_PARAMETER, ENUM

object NodeKind:
  given nodeKindToString: Conversion[NodeKind, String] = _.toString

enum EdgeKind:
  case CALL, DECLARATION, EXTEND, EXTEND_TYPE_ARGUMENT, PARAMETER, RETURN_TYPE, RETURN_TYPE_ARGUMENT, TYPE,
    TYPE_ARGUMENT, TYPE_PARAMETER, SUPER, OVERRIDE

object EdgeKind:
  given edgeKindToString: Conversion[EdgeKind, String] = _.toString
