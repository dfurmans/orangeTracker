package parser.dsl
import parser.adt.XMLCustom.{XMLDocument, XMLEmpty, XMLasBigSubStructureStringBase}
import parser.adt._
import parser.configuration.{AttributeConfiguration, SearchEngine, SearchEngineConfiguration}
import parser.generators.IdGenerators.IntegerIdGenerator
import parser.generators._

import scala.annotation.tailrec

case class TransformDocumentRules(elements : List[String],
                                  attributes: List[String],
                                  valuesIncluded: List[String],
                                  valuesExcluded: List[String]
                                 )
object TransformDocumentRules{

  val defaultTransformationRules: TransformDocumentRules = TransformDocumentRules(
    elements = SearchEngine.elementToSearchFor,
    attributes = SearchEngine.attributesToSearchFor,
    valuesIncluded = SearchEngine.defaultReqAttributeChunks.chunksAttributeNamesIncluded,
    valuesExcluded = SearchEngine.defaultReqAttributeChunks.chunksAttributeNameExcluded
  )

  val emptyRules: TransformDocumentRules = TransformDocumentRules( elements = List(), attributes = List(), List() , List())
}

// DSL for our ADT model - here is a place for new implementations and operation for our small DSL
trait Interpreter {
  val interpretAsString: XMLCustom => String
  val transformDocument: TransformDocumentRules => XMLCustom => XMLCustom
}

object Interpreters {

  val aXMLCustomXMLInterpreter: aXMLCustomXMLInterpreter = new aXMLCustomXMLInterpreter
  class aXMLCustomXMLInterpreter extends Interpreter {
    override val interpretAsString: XMLCustom => String = xmlCustom => {
      writeWholeDocument(
        xmlAsString(xmlCustom)
      )
    }

    override val transformDocument: TransformDocumentRules => XMLCustom => XMLCustom = trans => data => {
      data match {

        case XMLCustom.XMLEmpty => XMLCustom.XMLEmpty

        case XMLCustom.XMLDocument(_) if trans.attributes.isEmpty && trans.elements.isEmpty => data

        case XMLCustom.XMLDocument(aData) => {
          val idStateGenerator: IntegerIdGenerator = IntegerIdGenerator(0, Map())
          val chunkTransformed: Seq[(String, XMLElementChunks, List[XMLElementChunks])] = aData.map { x =>
            val rootName = x._1
            val alterParent: (List[XMLElementChunks], Id[Int, String]) = updateXMLEle(List(x._2), List(), trans, idStateGenerator)
            val alterKids: (List[XMLElementChunks], Id[Int, String]) = updateXMLEle(x._3, List(), trans, alterParent._2)
            (rootName, alterParent._1.head, alterKids._1)
          }
          XMLDocument(rawData = chunkTransformed)
        }
      }
    }

    def updateXMLEle(elements : List[XMLElementChunks], accEles :List[XMLElementChunks], trans: TransformDocumentRules, state: Id[Int, String]): (List[XMLElementChunks], Id[Int, String]) ={
      val result: (List[XMLElementChunks], Id[Int, String]) = elements match {
        case Nil =>
          (accEles, state)
        case x::tail => {
          val transRuleLambda: (XMLElementChunks, Id[Int, String]) => (XMLElementChunks, Id[Int, String]) = alterXMLElement(trans)
          val k = transRuleLambda(x, state)
          updateXMLEle(tail, accEles.appended(k._1),trans, k._2)
        }
      }
      result
    }

    val alterXMLElement: TransformDocumentRules => (XMLElementChunks, Id[Int, String]) => (XMLElementChunks, Id[Int, String]) = transformation => (input, state) => {
      if(transformation.elements.contains(input.tagName)){
        // check attributes
        val attributes: Map[String, String] = input.metaData.getOrElse(Map())
        if (transformation.attributes.map(x=> attributes.contains(x)).reduce((x,y)=>x||y)) {
          val dataToAlter = input
          val attributeValues = attributes.values.toList
            if (attributeValues.map(x => {
              transformation.valuesIncluded.map(y=> x.contains(y)).reduce((n,m) => n && m) &&
                transformation.valuesExcluded.map(y=> !x.contains(y)).reduce((n,m) => n && m)}).reduce((x,y) => x && y )){
            val newAttributeMap: (List[(String, Int)], Id[Int, String]) = recMap(attributes.toList, IdGenerators.generatorFunction, state)
            val toMApAttribute: Map[String, String] = newAttributeMap._1.map(x => x._1 -> x._2.toString).toMap
            val dataWithNewStateMonad : (XMLElementChunks, Id[Int, String]) = (dataToAlter.copy(metaData = Some(toMApAttribute)), newAttributeMap._2)
            dataWithNewStateMonad
          } else (input,state)
        } else (input, state)
      } else (input,state)
    }

  }

    def scanFor(configuration: SearchEngine, doc: XMLCustom): List[Option[XMLElementChunks]] = {
    doc match {
      case XMLEmpty => List.empty
      case XMLDocument(data) => {
        val elements: Seq[(String, XMLElementChunks, List[XMLElementChunks])] = data
        val checkFunctionForASingleElement = check(configuration)
        elements.flatMap { aSingleDataElements => {
          val parent: XMLElementChunks = aSingleDataElements._2
          val kids = aSingleDataElements._3
          val checkParents: Option[XMLElementChunks] = checkFunctionForASingleElement(parent)
          val checkKids: List[Option[XMLElementChunks]] = kids.map(k => checkFunctionForASingleElement(k))
          checkKids ::: List(checkParents)
        }}.toList}
    }
  }

  lazy val findXMLElementChunksForAttributeName : AttributeConfiguration => XMLElementChunks => Option[XMLElementChunks] = conf => e => {
    e.metaData match {
      case Some(a) => {
        val attributeValues = a.values.toList
        if (attributeValues.map(x => {
          conf.chunksAttributeNamesIncluded.map(y=> x.contains(y)).reduce((n,m) => n && m) &&
            conf.chunksAttributeNameExcluded.map(y=> !x.contains(y)).reduce((n,m) => n && m)}
        ).reduce((x,y) => x && y ))
          Some(e)
        else None
      }
      case _ => None
    }
  }

  // Rewriting data List[(String, String)] into to the List[String,Int] where second type parameter is evaluated by generator
  def recMap(data: List[(String, String)], functionGenerator: Int => Int, generator: Id[Int,String]): (List[(String, Int)], Id[Int,String]) ={
    val keys = data.map(kk=> kk._1)
    val values = data.map(kk=> kk._2)

    def recurs(data: (List[String], List[String]), acc: List[(String, Int)], generatorId: Id[Int,String]): (List[(String, Int)], Id[Int,String]) ={

      val subResult: (List[(String, Int)], Id[Int, String]) = data._1 match {
        case Nil => (acc,generator)
        case x::tail => {
          val xTail = tail
          data._2 match {
            case Nil => (acc, generator)
            case y::tail => {
              val yTail = tail
              val r: ((String, Int), Id[Int, String]) = changeForSingleTuple(functionGenerator, (x,y), generatorId)
              val accumulatorData: List[(String, Int)] = acc:+r._1
              val chunkResult: List[(String, Int)] = recurs(Tuple2(xTail, yTail), accumulatorData, r._2)._1
              (chunkResult, r._2)
            }
          }
        }
      }
      subResult
    }
    val finalResult: (List[(String, Int)], Id[Int, String]) = recurs(Tuple2(keys, values), List(), generator)
    finalResult
  }

  def changeForSingleTuple(functionGenerator: Int=>Int, t : (String, String), generators: Id[Int,String]): ((String, Int), Id[Int, String]) ={
    val nextGeneratorState = generators.generateNextId(functionGenerator)(t._1)
    (t._1 -> nextGeneratorState._1, nextGeneratorState._2)
  }

  lazy val check: SearchEngine => XMLElementChunks => Option[XMLElementChunks] = forSuchAConf => forSuchAnElement => {
    forSuchAConf match {
      case SearchEngineConfiguration(e,a) => {
        forSuchAnElement.metaData match {
          case Some(data) => {
            e match {
              case  _::_ =>{
                val attributedFound = if(a.nonEmpty) {
                  a.map { k =>
                    data.keySet.contains(k)
                  }.reduce((x, y) => x || y)
                } else true
                val forTagName = e.map { k =>
                  forSuchAnElement.tagName == k
                }.reduce((a, b) => a || b)
                if (attributedFound && forTagName) Some(forSuchAnElement) else None}
              case _=> Some(forSuchAnElement)
            }
          }
          case None => None}
      }
      case _ => None
    }
  }

  val xmlAsString: XMLCustom => XMLasBigSubStructureStringBase = xml => {
    xml match {
      case XMLCustom.XMLEmpty => Seq()
      case XMLCustom.XMLDocument(data) => {
        data.map { x =>
        {
          val topLevelTag = x._1
          val startTag: String = writeStartTag(x._2.tagName)
          val innerTags: List[String] = x._3.map { inner =>
          {
            inner.xmlType match {
              case ElementType | SpecialElementType =>
                writeInnerElements(inner.tagName,
                  inner.content,
                  inner.metaData)
              case TextType => writeContentOnly(inner.content)
              case _ => ""
            }
          }
          }
          val endTag: String = writeEndTag(x._2.tagName)
          (topLevelTag, startTag, innerTags, endTag)
        }
        }
      }
    }
  }

  def writeContentOnly(text: Option[String]): String = {
    text match {
      case Some(e) if e.nonEmpty => e.mkString
      case _    => ""
    }
  }

  def writeInnerElements(aTag: String,
                         text: Option[String],
                         attribute: Option[Map[String,String]]): String = {
    text match {
      case Some(aContent)  => s"""<$aTag${writeAttributes(attribute)}>$aContent</$aTag>"""
      case None            => s"""<$aTag${writeAttributes(attribute)}/>"""
    }
  }

  def writeAttributes(attribute: Option[Map[String, String]]): String = {
    attribute match {
      case Some(a) =>
        a.map(aTuple => " " + aTuple._1 + "=" + "\"" + aTuple._2 + "\"" ).mkString
      case None    => ""
    }
  }

  def writeStartTag(aTag: String): String = s"<$aTag>"

  def writeEndTag(aTag: String): String = s"</${aTag.trim}>".trim

  def writeWholeDocument(data: XMLasBigSubStructureStringBase): String =
    s"""<html>${render(data, new StringBuilder())}</html>"""


  @tailrec
  def render(data: XMLasBigSubStructureStringBase,
             acc: StringBuilder): String = {
    data match {
      case x :: tail => {
        val chunk: String = s"""${x._2} ${x._3.filter(p => p.nonEmpty).mkString} ${x._4}"""
        render(tail, acc.append("\n    ").append(chunk).append(""))
      }
      case Nil => acc.mkString
    }
  }
}