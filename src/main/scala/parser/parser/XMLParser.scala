package parser.parser

import parser.adt.XMLCustom.RootParentKidsXML
import parser.adt._

import scala.util.{Failure, Success, Try}
import scala.xml._

trait Parser{

  def parse(doc: scala.xml.Elem): Parsers.ParserConfiguration => Either[ParsingFailure, XMLCustom]

  /**
   * CAUTION :: The SCALA API define NodeSeq is a single node of sequence length 1
   * because of that we always return a Single Tuple2[XMLElementChunks, List[XMLElementChunks]]
   * All is baked only on projection function \\(that: String): NodeSeq from scala.xml package.
   *
   * @param anXMLElementByName the tag to search for
   * @param doc HTML as a SCALA XML doc
   * @return  a Seq[RootParentKidsXML] where RootParentKidsXML = (String, XMLElementChunks, List[XMLElementChunks])
   */
  def chunkMetaXMLData(anXMLElementByName: String, doc: Elem): Seq[RootParentKidsXML] = {
    val anElementByName: NodeSeq = doc \\ anXMLElementByName

    anElementByName.map(s => {
      val parent = lensSingleNote(s)
      val kids: List[XMLElementChunks] = s.nonEmptyChildren.map(n => lensSingleNote(n)).toList
      (anXMLElementByName, parent, kids)
    })
  }

  private def lensSingleNote(aSingleNode: Node): XMLElementChunks = {
    aSingleNode match {
      case e: Elem      => XMLElementChunks(
          e.label,
          if (e.attributes.nonEmpty) Some(e.attributes.asAttrMap) else None,
          if (e.text.nonEmpty) Some(e.text) else None,
          ElementType)
      case t: Text       => XMLElementChunks(
          t.label,
          None,
          if (t.text.nonEmpty) Some(t.text) else None,
          TextType)
      case s: SpecialNode => XMLElementChunks(s.label, None, Some(s.text), SpecialElementType)
      case _              => XMLElementChunks("", None, None, NotSupportedElementType)
    }
  }

}

object Parsers {

  case class ParserConfiguration(tagsToFind: List[String])
  object ParserConfiguration {

    def apply(tagsToFind: List[String]): ParserConfiguration = new ParserConfiguration(tagsToFind)

    val defaultConfiguration: ParserConfiguration = ParserConfiguration(
      List("head", "body")
    )
  }

  val anXMLParser: Parser = new Parser {

    override def parse(
                        input: Elem): ParserConfiguration => Either[ParsingFailure, XMLCustom] =
      awareParserConfiguration => {
        fromTry(
          Try {
            val xmlChunksSequence: Seq[(String, XMLElementChunks, List[XMLElementChunks])] =
              awareParserConfiguration.tagsToFind.flatMap { aSingleTag =>
                chunkMetaXMLData(aSingleTag, input)}
            XMLCustom.XMLDocument(xmlChunksSequence)
          }
        )
      }

    def fromTry(t: Try[XMLCustom]): Either[ParsingFailure, XMLCustom] =
      t match {
        case Success(anXML)   => Right(anXML)
        case Failure(anError) => Left(ParsingFailure(anError.getMessage))
      }
  }
}
