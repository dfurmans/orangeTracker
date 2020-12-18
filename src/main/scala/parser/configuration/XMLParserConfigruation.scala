package parser.configuration

trait SearchEngine
// Empty list means -> search for all elements
case class SearchEngineConfiguration(elements: List[String],
                                     attributes: List[String])
    extends SearchEngine
class AttributeConfiguration(val chunksAttributeNamesIncluded: List[String],
                             val chunksAttributeNameExcluded: List[String])

object SearchEngine {
  val valuesToLookingFor = List("oranges")
  val valuesToNotLookingFor = List("bananas")
  val elementToSearchFor = List("img", "a")
  val attributesToSearchFor = List("src", "href")

  val defaultSearchConfiguration: SearchEngineConfiguration =
    SearchEngineConfiguration(
      elements = elementToSearchFor,
      attributes = attributesToSearchFor
    )

  val allImgAndA =
    SearchEngineConfiguration(elements = elementToSearchFor, attributes = attributesToSearchFor)

  val defaultReqAttributeChunks = new AttributeConfiguration(
    chunksAttributeNamesIncluded = valuesToLookingFor,
    chunksAttributeNameExcluded = valuesToNotLookingFor
  )
}
