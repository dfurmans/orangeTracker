import parser.dsl._
import parser.parser._
import parser.adt.XMLCustom.XMLEmpty
import parser.adt._
import parser.adt.interpreters.PathTrackerResult
import parser.configuration.{AttributeConfiguration, SearchEngine, SearchEngineConfiguration}
import parser.generators.IdGenerators._

import scala.xml._

  /**
   * XMLCustom Parser conf
   */
  object ParserCLIConfig {
    val parserConfiguration: Parsers.ParserConfiguration = Parsers.ParserConfiguration.defaultConfiguration
  }

/**
 * Search engine conf
 */
object SearchEngineCLIConfig{
  val elementsToFind: SearchEngineConfiguration = SearchEngine.allImgAndA
  val attributeChunksNamesExcludeInclude: AttributeConfiguration = SearchEngine.defaultReqAttributeChunks
  val defaultSearchConfiguration: SearchEngineConfiguration = SearchEngine.defaultSearchConfiguration
}

/**
 * ID generator conf
 */
object IDGeneratorCLIConfig{
  val functionForGeneratesID: Int => Int = parser.generators.IdGenerators.generatorFunction
}

object LinkTracker {
  def parseDoc(doc: Elem): PathTrackerResult = {

    val parserD: Parsers.ParserConfiguration => Either[ParsingFailure, XMLCustom] = Parsers.anXMLParser.parse(doc)
    val parserConfiguration: Parsers.ParserConfiguration = Parsers.ParserConfiguration.defaultConfiguration
    val tryParseTheDocument: Either[ParsingFailure, XMLCustom] = parserD(parserConfiguration)
    val parsed: XMLCustom = tryParseTheDocument.getOrElse(XMLCustom.XMLEmpty)

    PathTrackerResult(
      trackedDoc = {
        val transformationRules: TransformDocumentRules = TransformDocumentRules.defaultTransformationRules
        val transformDocumentWithRules: XMLCustom = Interpreters.aXMLCustomXMLInterpreter.transformDocument(transformationRules)(parsed)
        Interpreters.writeWholeDocument(Interpreters.xmlAsString(transformDocumentWithRules))
      },
      encodedPaths = {
          val allImgAndAXMLElements: Seq[XMLElementChunks] = Interpreters.scanFor(SearchEngineCLIConfig.elementsToFind, tryParseTheDocument.getOrElse(XMLEmpty)).filter(_.isDefined).flatten
          val findElementsByAttributeConfiguration: XMLElementChunks => Option[XMLElementChunks] = Interpreters.findXMLElementChunksForAttributeName(SearchEngine.defaultReqAttributeChunks)
          val findElements: Seq[XMLElementChunks] = allImgAndAXMLElements.flatMap(x => findElementsByAttributeConfiguration(x))
          val allMetaDataForElements: List[(String, String)] = findElements.flatMap(x => x.metaData.getOrElse(Map())).toList
          val flipKeyValue = for ((k,v) <-allMetaDataForElements.toMap) yield (v,k)
          val listOfMap = flipKeyValue.toList
          val encodedPaths: Seq[(String, Int)] = Interpreters.recMap(listOfMap, IDGeneratorCLIConfig.functionForGeneratesID, IntegerIdGenerator(0, Map()))._1
          // doing MAP Here for UNIQ PurposeS!
           encodedPaths.map(x => (x(1), x(0))).toMap
      },
      uniquePaths = Interpreters.scanFor(SearchEngineCLIConfig.defaultSearchConfiguration, parsed).count(_.isDefined),
      totalPaths  = Interpreters.scanFor(SearchEngineCLIConfig.elementsToFind, parsed).count(_.isDefined))
  }
}