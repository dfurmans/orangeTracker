package parser.adt

sealed abstract class XMLCustom
object XMLCustom {
  type RootParentKidsXML = (String, XMLElementChunks, List[XMLElementChunks])
  type XMLasBigSubStructureStringBase = Seq[(String, String, List[String], String)]
  case object XMLEmpty extends XMLCustom
  case class XMLDocument(rawData: Seq[RootParentKidsXML]) extends XMLCustom
}

final case class ParsingFailure(message: String)
sealed trait XmlType
case object ElementType extends XmlType
case object TextType extends XmlType
case object SpecialElementType extends XmlType
case object NotSupportedElementType extends XmlType
case class XMLElementChunks(tagName: String,
                            metaData: Option[Map[String, String]],
                            content: Option[String],
                            xmlType: XmlType)
    extends XmlType