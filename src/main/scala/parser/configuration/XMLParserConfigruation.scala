package parser.configuration

trait SearchEngine
// Empty list means -> search for all elements
case class SearchEngineConfiguration(elements: List[String],
                                     attributes: List[String])
    extends SearchEngine
class AttributeConfiguration(val chunksAttributeNamesIncluded: List[String],
                             val chunksAttributeNameExcluded: List[String])

object SearchEngine {
  val valuesToLookingFor: List[String] = List("oranges")
  val valuesToNotLookingFor: List[String] = List("bananas")
  val elementToSearchFor: List[String] = List("img", "a")
  val attributesToSearchFor: List[String] = List("src", "href")

  val defaultSearchConfiguration: SearchEngineConfiguration =
    SearchEngineConfiguration(
      elements = elementToSearchFor,
      attributes = attributesToSearchFor
    )

  val allImgAndA: SearchEngineConfiguration =
    SearchEngineConfiguration(elements = elementToSearchFor, attributes = attributesToSearchFor)

  val defaultReqAttributeChunks = new AttributeConfiguration(
    chunksAttributeNamesIncluded = valuesToLookingFor,
    chunksAttributeNameExcluded = valuesToNotLookingFor
  )
}
