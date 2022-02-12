package com.virtuslab.semanticgraphs.javaparser.extractor.utils

import com.virtuslab.semanticgraphs.javaparser.extractor.{EdgeKind, NodeKind}
import com.virtuslab.semanticgraphs.javaparser.extractor.utils.TooltipUtils.*
import com.virtuslab.semanticgraphs.parsercommon.toPath
import com.virtuslab.semanticgraphs.proto.model.graphnode.{Edge, Location}

import ch.qos.logback.classic.Logger
import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, PrimitiveType, Type, TypeParameter}
import com.github.javaparser.ast.{Node, PackageDeclaration}
import com.github.javaparser.ast.body.*
import com.github.javaparser.ast.expr.*
import com.github.javaparser.ast.nodeTypes.*
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt
import com.github.javaparser.ast.AccessSpecifier
import com.github.javaparser.resolution.declarations.*
import com.github.javaparser.resolution.declarations.ResolvedTypeParameterDeclaration.Bound
import com.github.javaparser.resolution.types.{ResolvedReferenceType, ResolvedType, ResolvedTypeVariable}
import com.github.javaparser.resolution.Resolvable
import com.github.javaparser.symbolsolver.javaparsermodel.declarations.{
  JavaParserEnumConstantDeclaration,
  JavaParserFieldDeclaration,
  JavaParserParameterDeclaration,
  JavaParserVariableDeclaration
}

import java.io.File
import java.nio.file.Path
import java.util
import java.util.Optional
import scala.annotation.targetName
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.language.implicitConversions
import scala.reflect.{classTag, ClassTag}
import scala.util.{Success, Try}

extension (classLikeDeclaration: ClassLikeDeclaration)(using
  logger: Logger
) {
  def getQualifiedSignature: Option[String] = classLikeDeclaration.getFullyQualifiedName.toScala

  /**
    * @param declaration
    *   a given declaration to be checked
    * @return
    *   true if the given declaration is an inner declaration for this class/interface, false otherwise
    */
  def checkIfInnerDeclaration(declaration: BodyDeclaration[_]): Boolean = (for {
    parentNode <- declaration.getParentNode.toScala
    parentTd <- parentNode match {
      case td: TypeDeclaration[_] => Some(td)
      case _                      => None
    }
    parentFqn <- parentTd.getFullyQualifiedName.toScala
    fqn <- classLikeDeclaration.getFullyQualifiedName.toScala
  } yield parentFqn == fqn).getOrElse(false)

  def constructorDeclarations: Seq[ConstructorDeclaration] =
    classLikeDeclaration.allNodesOf[ConstructorDeclaration].filter(classLikeDeclaration.checkIfInnerDeclaration)
  def methodDeclarations: Seq[MethodDeclaration] =
    classLikeDeclaration.allNodesOf[MethodDeclaration].filter(classLikeDeclaration.checkIfInnerDeclaration)
  def methodLikeDeclarations: Seq[MethodLikeDeclaration] = constructorDeclarations ++ methodDeclarations
  def variableDeclarations: Seq[VariableDeclarator] =
    classLikeDeclaration.getFields.asScala.flatMap(_.getVariables.asScala).toSeq
  def initializerDeclarations: Seq[InitializerDeclaration] = classLikeDeclaration.allNodesOf[InitializerDeclaration]

  def implementedTypes: Seq[ClassOrInterfaceType] = classLikeDeclaration.getImplementedTypes.asScala.toSeq

  def inheritedTypes: Seq[ClassOrInterfaceType] =
    implementedTypes ++ classOrInterfaceDeclarationOption.toSeq.flatMap(_.extendedTypes)

  def extendEdges(uri: String): Seq[Edge] = for {
    extendedType <- classLikeDeclaration.inheritedTypes
    sourceNode = extendedType.simpleNameNode.getOrElse(extendedType)
    qualifiedName = extendedType.resolveQualifiedNameOption.getOrElse(extendedType.getNameAsString)
  } yield sourceNode.edge(targetId = qualifiedName, edgeKind = EdgeKind.EXTEND, uri = uri)

  @targetName("variableDeclarationEdges_ClassLikeDeclaration")
  def variableDeclarationEdges(uri: String): Seq[Edge] = {
    (classLikeDeclaration.variableDeclarations ++ classLikeDeclaration.initializerVariableDeclarations).flatMap {
      variableDeclarator =>
        for {
          simpleName <- variableDeclarator.simpleNameNode
          signature <- variableDeclarator.getQualifiedSignature(uri)
        } yield simpleName.edge(targetId = signature, edgeKind = EdgeKind.DECLARATION, uri)
    }
  }

  def initializerVariableDeclarations: Seq[VariableDeclarator] =
    classLikeDeclaration.initializerDeclarations.filter(_.isStatic).flatMap(_.variableDeclarations)

  def methodLikeDeclarationEdges(uri: String): Seq[Edge] =
    classLikeDeclaration.methodLikeDeclarations.flatMap(_.declarationEdge(uri))

  def isInnerDeclaration: Boolean = classLikeDeclaration.isNestedType && !classLikeDeclaration.isStatic

  def classLikeDeclarations: Seq[ClassLikeDeclaration] = classLikeDeclaration.allNodesOf[ClassOrInterfaceDeclaration] ++
    classLikeDeclaration.allNodesOf[EnumDeclaration]

  def innerDeclarationEdges(uri: String): Seq[Edge] = classLikeDeclaration.classLikeDeclarations
    .filter(innerDeclaration =>
      innerDeclaration.isInnerDeclaration && innerDeclaration.getQualifiedSignature != classLikeDeclaration.getQualifiedSignature
    )
    .flatMap { innerDeclaration =>
      for {
        simpleName <- innerDeclaration.simpleNameNode
        signature <- innerDeclaration.getQualifiedSignature
      } yield simpleName.edge(targetId = signature, edgeKind = EdgeKind.DECLARATION, uri)
    }

  def initializerCallEdges(uri: String): Seq[Edge] =
    classLikeDeclaration.initializerDeclarations.filter(_.isStatic).flatMap(_.callEdges(uri))

  def declarationEdges(uri: String): Seq[Edge] = classLikeDeclaration.methodLikeDeclarationEdges(uri) ++
    classLikeDeclaration.variableDeclarationEdges(uri) ++
    classLikeDeclaration.innerDeclarationEdges(uri) ++
    classLikeDeclaration.enumDeclarationOption.toSeq.flatMap(_.enumConstantDeclarationEdges(uri))

  def classOrInterfaceDeclarationOption: Option[ClassOrInterfaceDeclaration] = classLikeDeclaration match {
    case coid: ClassOrInterfaceDeclaration => Some(coid)
    case _                                 => None
  }

  def enumDeclarationOption: Option[EnumDeclaration] = classLikeDeclaration match {
    case ed: EnumDeclaration => Some(ed)
    case _                   => None
  }

  @targetName("classLikeLOC")
  def getLOC: String =
    classLikeDeclaration.getRange.toScala.map(range => range.end.line - range.begin.line).getOrElse(0).toString

}

extension (coid: ClassOrInterfaceDeclaration)(using
  logger: Logger
) {
  def kind: NodeKind = if (coid.isInterface) NodeKind.TRAIT else NodeKind.CLASS

  def extendedTypes: Seq[ClassOrInterfaceType] = coid.getExtendedTypes.asScala.toSeq

  def extendTypeArgumentEdges(uri: String): Seq[Edge] = {
    for {
      extendedType <- coid.inheritedTypes
      typeArgument <- extendedType.typeArguments
      resolvedTypeArgument <- typeArgument.resolveOption
    } yield typeArgument.edge(
      targetId = resolvedTypeArgument.getQualifiedName,
      edgeKind = EdgeKind.EXTEND_TYPE_ARGUMENT,
      uri = uri
    )
  }

  def getDeclarationAsHTML: String = {

    val sb = mutable.StringBuilder()
    sb.append(coid.getAnnotations.asScala.map(_.toString.asAnnotation + " <br>").mkString)
    sb.append(coid.getAccessSpecifier.asString.appended(' ').asModifier)
    if coid.isInterface then {
      sb.append("interface ".asModifier)
    } else {
      if coid.isAbstract then sb.append("abstract ".asModifier)
      if coid.isFinal then sb.append("final ".asModifier)
      sb.append("class ".asModifier)
    }
    sb.append(coid.getName)
    val typeParameters = coid.scopeTypeParameters
    if !typeParameters.isEmpty then {
      sb.append("&lt;")
      sb.append(typeParameters.map(asTypeParameter).mkString(", ").asModifier)
      sb.append("&gt;")
    }

    if !coid.extendedTypes.isEmpty then {
      sb.append(" extends ".asModifier)
      val extendedTypesNames = coid.extendedTypes.map { extendedType =>
        TooltipUtils.colorGenericTypes(typeParameters, extendedType.toString.replace("<", "&lt;").replace(">", "&gt;"))
      }
      sb.append(extendedTypes.mkString(", "))
    }

    if !coid.implementedTypes.isEmpty then {
      sb.append("<br>implements ".asModifier)
      val implementedTypes = coid.implementedTypes.map { implementedType =>
        TooltipUtils
          .colorGenericTypes(typeParameters, implementedType.toString.replace("<", "&lt;").replace(">", "&gt;"))
      }
      sb.append(implementedTypes.mkString(", "))
    }

    TooltipUtils.colorizeSpecialKeywords(sb.result).strip
  }

}

extension (e: EnumDeclaration)(using
  logger: Logger
) {
  def kind: NodeKind = NodeKind.ENUM

  def enumConstants: Seq[EnumConstantDeclaration] = e.getEntries.asScala.toSeq

  def enumConstantDeclarationEdges(uri: String): Seq[Edge] = for {
    ec <- e.enumConstants
    signature <- ec.getQualifiedSignature
    simpleNameNode <- ec.simpleNameNode
  } yield simpleNameNode.edge(targetId = signature, edgeKind = EdgeKind.DECLARATION, uri)

  def getDeclarationAsHTML: String = "enum ".asModifier + e.getName
}

extension (variable: VariableDeclarator)(using
  logger: Logger
) {

  def kind: NodeKind = {
    if (
      variable.getParentNode.toScala.exists {
        case field: FieldDeclaration               => field.isFinal
        case variableExpr: VariableDeclarationExpr => variableExpr.isFinal
      }
    ) NodeKind.VALUE
    else NodeKind.VARIABLE
  }

  def getQualifiedSignature(uri: String): Option[String] = for {
    ancestorSignature <- variable.closestPossiblyDeclaringAncestorSignature
    variableSignature = variable.signatureWithCoordinates
  } yield s"$ancestorSignature?$variableSignature"

  def declarationEdge(uri: String): Option[Edge] = for {
    simpleNameNode <- variable.simpleNameNode
    qs <- variable.getQualifiedSignature(uri)
  } yield simpleNameNode.edge(targetId = qs, edgeKind = EdgeKind.DECLARATION, uri = uri)

  private def getAccessModifiers(decl: Node): String = decl match {
    case f: (FieldDeclaration | VariableDeclarationExpr) => f.getAccessSpecifier.asString.appended(' ').asModifier
    case _                                               => ""
  }

  private def getModifiersString(node: Node): String = {
    val sb = mutable.StringBuilder()
    node match {
      case f: FieldDeclaration =>
        if f.isStatic then sb.append("static ".asModifier)
        if f.isFinal then sb.append("final ".asModifier)
      case v: VariableDeclarationExpr => if v.isFinal then sb.append("final ".asModifier)
    }
    sb.result
  }

  def getDeclarationAsHTML: String = {
    val sb = mutable.StringBuilder()
    variable.getParentNode.asScala match {
      case Some(decl) =>
        sb.append(getAccessModifiers(decl))
        sb.append(getModifiersString(decl))
        sb.append(TooltipUtils.colorGenericTypes(variable.scopeTypeParameters, variable.getType.toString))
        sb.append(" ")
        sb.append(variable.getName.toString.asClassField)
      case None =>
    }
    val splitIndex = variable.getParentNode.asScala.map(_.toString.indexOf("=")).getOrElse(-1)
    if (splitIndex != -1 && variable.getParentNode.asScala.isDefined) {
      val rhs = variable.getParentNode.get.toString.splitAt(splitIndex)._2
      sb.append(" ")
      sb.append(rhs)
    }
    TooltipUtils.colorizeSpecialKeywords(sb.result)

  }

}

extension (node: Node with NodeWithSimpleName[_ <: Node] with Resolvable[_ <: ResolvedValueDeclaration])(using
  logger: Logger
) {

  def signatureWithCoordinates: String = {
    val coordinates = node.simpleNameCoordinates.orElse(node.coordinates).getOrElse("UNKNOWN")
    val name = node.getNameAsString
    s"$name@$coordinates"
  }

  def isParameterOf(declaration: ClassLikeDeclaration | MethodLikeDeclaration): Boolean = declaration match {
    case methodOrConstructor: (MethodDeclaration | ConstructorDeclaration) =>
      methodOrConstructor.getParameters.asScala.exists(_.getNameAsString == node.getNameAsString)
    case _ => false
  }

  def getQualifiedSignature: Option[String] = for {
    resolvedNameExpr <- node.resolveOption
    (wrappedNode: Node, fieldName) <- resolvedNameExpr match {
      case jpfd: JavaParserFieldDeclaration =>
        Some(jpfd.getWrappedNode -> jpfd.getVariableDeclarator.signatureWithCoordinates)
      case jppd: JavaParserParameterDeclaration => Some(jppd.getWrappedNode -> jppd.getWrappedNode.signature)
      case jpvd: JavaParserVariableDeclaration =>
        Some(jpvd.getWrappedNode -> jpvd.getVariableDeclarator.signatureWithCoordinates)
      case jpecd: JavaParserEnumConstantDeclaration =>
        Some(jpecd.getWrappedNode -> jpecd.getWrappedNode.getNameAsString)
      case _ => None
    }
    ancestorNode <- wrappedNode.closestPossiblyDeclaringAncestor

    ancestorSignature <- wrappedNode.closestPossiblyDeclaringAncestorSignature
    isParameter = node.isParameterOf(ancestorNode)
    separator = if isParameter then "!" else "?"
  } yield s"$ancestorSignature$separator$fieldName"

}

extension (explConstructorInv: ExplicitConstructorInvocationStmt)(using
  logger: Logger
) {
  def isSuperStmt: Boolean = explConstructorInv.toString.startsWith("super")
}

extension (parameter: Parameter)(using
  logger: Logger
) {

  def kind: NodeKind = (parameter.treatedAsVariable, parameter.isFinal) match {
    case (true, true)  => NodeKind.VALUE
    case (true, false) => NodeKind.VARIABLE
    case (false, _)    => NodeKind.PARAMETER
  }

  def treatedAsVariable: Boolean = (
    for {
      ancestorNode <- parameter.closestPossiblyDeclaringAncestor
      treatedAsVariable = !parameter.isParameterOf(ancestorNode)
    } yield treatedAsVariable
  ).getOrElse(true)

  def signature: String = (
    for {
      coordinates <- parameter.simpleNameCoordinates.orElse(parameter.coordinates)
      name = parameter.getNameAsString
      treatedAsVariable = parameter.treatedAsVariable
    } yield if treatedAsVariable then s"$name@$coordinates" else name
  ).getOrElse(parameter.getNameAsString)

  def getQualifiedSignature: Option[String] = for {
    ancestorSignature <- parameter.closestPossiblyDeclaringAncestorSignature
    signature = parameter.signature
    nodeTypeMarker = if parameter.treatedAsVariable then "?" else "!"
  } yield s"$ancestorSignature$nodeTypeMarker$signature"

  def parameterEdge(uri: String): Option[Edge] = for {
    simpleNameNode <- parameter.simpleNameNode
    signature <- parameter.getQualifiedSignature
  } yield simpleNameNode.edge(targetId = signature, edgeKind = EdgeKind.PARAMETER, uri = uri)

  def declarationEdge(uri: String): Option[Edge] = for {
    simpleNameNode <- parameter.simpleNameNode
    qs <- parameter.getQualifiedSignature
  } yield simpleNameNode.edge(targetId = qs, edgeKind = EdgeKind.DECLARATION, uri = uri)

  def getDeclarationAsHTML: String = {
    val annotations = parameter.getAnnotations.asScala.map(_.toString.asAnnotation + " ").mkString
    val parameterType = TooltipUtils.colorGenericTypes(parameter.scopeTypeParameters, parameter.getType.toString + " ")
    val parameterName = parameter.getName.toString
    TooltipUtils.colorizeSpecialKeywords(annotations + parameterType + parameterName)
  }

}

extension (methodLike: MethodLikeDeclaration)(using
  logger: Logger
) {

  def getQualifiedSignature: String =
    methodLike.resolveOption.flatMap(_.qualifiedSignatureOption).getOrElse(methodLike.getNameAsString)

  def kind: NodeKind = methodLike match {
    case _: ConstructorDeclaration => NodeKind.CONSTRUCTOR
    case _: MethodDeclaration      => NodeKind.METHOD
  }

  def typeQualifiedSignature: String = methodLike match {
    case m: MethodDeclaration      => m.getTypeFullyQualifiedName
    case c: ConstructorDeclaration => c.closestPossiblyDeclaringAncestorSignature.getOrElse(c.getNameAsString)
  }

  def directParameters: Seq[Parameter] = methodLike.getParameters.asScala.toSeq

  def allParameters: Seq[Parameter] = methodLike.allNodesOf[Parameter]

  def declaredVariables: Seq[VariableDeclarator] =
    methodLike.allNodesOf[VariableDeclarationExpr].flatMap(_.getVariables.asScala)

  def childParametersTreatedAsVariables = methodLike.allNodesOf[Parameter].filter(_.treatedAsVariable)

  def parameterEdges(uri: String): Seq[Edge] = methodLike.directParameters.flatMap(_.parameterEdge(uri))

  def variableDeclarationEdges(uri: String): Seq[Edge] = methodLike.declaredVariables.flatMap(_.declarationEdge(uri)) ++
    methodLike.childParametersTreatedAsVariables.flatMap(_.declarationEdge(uri))

  def returnTypeEdge(uri: String): Option[Edge] = methodLike.typeNameNode
    .orElse(methodLike.typeNode)
    .orElse(methodLike.simpleNameNode)
    .map(_.edge(targetId = methodLike.typeQualifiedSignature, EdgeKind.RETURN_TYPE, uri))

  def returnTypeParameterEdges(uri: String): Seq[Edge] = methodLike match {
    case c: ConstructorDeclaration => Seq.empty
    case m: MethodDeclaration      => m.typeArgumentEdges(uri)
  }

  def declarationEdge(uri: String): Option[Edge] = methodLike.simpleNameNode
    .orElse(Some(methodLike))
    .map(_.edge(targetId = methodLike.getQualifiedSignature, EdgeKind.DECLARATION, uri))

  def getDeclarationAsHTML: String = {
    val sb = mutable.StringBuilder()
    sb.append(methodLike.getAnnotations.asScala.map(_.toString + " <br>").mkString)
    sb.append(methodLike.getAccessSpecifier.asString.appended(' ').asModifier)
    val typeParameters = methodLike.scopeTypeParameters

    methodLike match {
      case m: MethodDeclaration =>
        if m.isStatic then sb.append("static ".asModifier)
        if m.isAbstract then sb.append("abstract ".asModifier)
        if m.isFinal then sb.append("final ".asModifier)
        if m.isNative then sb.append("native ".asModifier)
        if m.isSynchronized then sb.append("synchronized ".asModifier);

        if !typeParameters.isEmpty then {
          sb.append("&lt;")
          sb.append(typeParameters.map(asTypeParameter).mkString(", ").asModifier)
          sb.append("&gt; ")
        }

        sb.append(
          TooltipUtils.colorGenericTypes(typeParameters, m.getTypeAsString.replace("<", "&lt;").replace(">", "&gt;"))
        )
        sb.append(" ")

      case c: ConstructorDeclaration =>
    }
    sb.append(methodLike.getName.toString.asMethod)
    sb.append("(");
    val parameters = methodLike.getParameters.asScala.map { parameter =>
      TooltipUtils.colorGenericTypes(
        typeParameters,
        parameter.toString.replace("<", "&lt;").replace(">", "&gt;")
      )
    }
    sb.append(parameters.mkString(", "))
    sb.append(")");
    val thrownExceptions = methodLike.getThrownExceptions.asScala.map(_.toString)
    if !thrownExceptions.isEmpty then {
      sb.append(" throws ".asModifier)
      sb.append(thrownExceptions.mkString(", "))
    }
    TooltipUtils.colorizeSpecialKeywords(TooltipUtils.colorAnnotations(sb.result.replace(",", ",".asModifier)))
  }

  def overrideEdges(uri: String): Seq[Edge] = {
    methodLike match {
      case method: MethodDeclaration =>
        for {
          declaringClass <- method.getDeclaringClass.toSeq
          inheritedType: ClassOrInterfaceType <- declaringClass.inheritedTypes
          resolvedType <- inheritedType.resolveOption.toSeq
          resolvedReferenceType <- (resolvedType match {
            case rrt: ResolvedReferenceType => Some(rrt)
            case _                          => None
          }).toSeq
          methodVisibleToInheritors <- resolvedReferenceType.allMethodsVisibleToInheritors
          if methodVisibleToInheritors.toAst.toScala.exists(_.hasTheSameSignature(method))
          to <- Try(methodVisibleToInheritors.getQualifiedSignature).withLogging().toOption
        } yield Edge(
          to = to,
          `type` = EdgeKind.OVERRIDE,
          location = method.simpleNameLocation(uri)
        )
      case _ => Seq.empty
    }
  }

  def getLOC: String = methodLike.getRange.toScala.map(range => range.end.line - range.begin.line).getOrElse(0).toString

}

extension (stmt: ExplicitConstructorInvocationStmt)(using
  logger: Logger
) {

  def superLocation(uri: String): Option[Location] = {
    if stmt.isSuperStmt then
      stmt.location(uri).map(l => l.copy(endLine = l.startLine, endCharacter = l.startCharacter + 5))
    else stmt.location(uri)
  }

}

extension (packageDeclaration: PackageDeclaration)(using
  logger: Logger
) {
  def asPath: Path = packageDeclaration.getNameAsString.split("\\.").mkString(File.separator).toPath
}

extension (initializerDeclaration: InitializerDeclaration)(using
  logger: Logger
) {

  def getQualifiedSignature(orderNumber: Int): Option[String] =
    initializerDeclaration.closestPossiblyDeclaringAncestorSignature

  def variableDeclarations: Seq[VariableDeclarator] =
    initializerDeclaration.allNodesOf[VariableDeclarationExpr].flatMap(_.getVariables.asScala)

  def variableDeclarationEdges(uri: String): Seq[Edge] =
    initializerDeclaration.variableDeclarations.flatMap(_.declarationEdge(uri))

}

extension (method: MethodDeclaration)(using
  logger: Logger
) {

  def hasTheSameSignature(otherMethod: MethodDeclaration): Boolean = {
    otherMethod.getSignature == method.getSignature &&
    otherMethod.isStatic == method.isStatic &&
    otherMethod.getType == method.getType
  }

  def getDeclaringClass: Option[ClassOrInterfaceDeclaration] = method.getParentNode.toScala.flatMap {
    case coid: ClassOrInterfaceDeclaration => Some(coid)
    case _                                 => None
  }

}
