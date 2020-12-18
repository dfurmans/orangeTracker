import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import parser.adt.{ParsingFailure, XMLCustom}
import parser.dsl.{Interpreters, TransformDocumentRules}
import parser.parser.Parsers

import scala.xml.Elem

class LinkTrackerSpec extends AsyncFlatSpec with Matchers {
  val res1 = LinkTracker.parseDoc(Sample1.originalDoc)

  it should "count the total unique img/a paths in a document" in {
    res1.uniquePaths should be (Sample1.expectedUniquePaths)
  }

  it should "provide a set of encoded paths for a document as a map of encoding id to path" in {
    res1.encodedPaths should be (Sample1.uniqueEncodedPathWithNumbers)
  }

  it should "provide a string version of the document with all img/a paths encoded" in {
    sanitizeABigXMLString(res1.trackedDoc) should be (sanitizeABigXMLString(Sample1.expectedDoc))
  }

  it should "a transformation without rule should return the same XML structure" in {

    val parserD: Parsers.ParserConfiguration => Either[ParsingFailure, XMLCustom] = Parsers.anXMLParser.parse(Sample1.originalDoc)
    val parserConfiguration: Parsers.ParserConfiguration = Parsers.ParserConfiguration.defaultConfiguration
    val tryParseTheDocument: Either[ParsingFailure, XMLCustom] = parserD(parserConfiguration)
    val parsed: XMLCustom = tryParseTheDocument.getOrElse(XMLCustom.XMLEmpty)
    val rules = TransformDocumentRules.emptyRules

    parsed should be (Interpreters.aXMLCustomXMLInterpreter.transformDocument(rules)(parsed))
  }

  def sanitizeABigXMLString(xml: String) = {
    val onlyOneTheLastOneUglyRegexp =  "\\s+"
    val charactersToSanitize = ""
    xml.replaceAll(onlyOneTheLastOneUglyRegexp,charactersToSanitize)
  }

}

object Sample1 {
  val originalDoc: Elem =
<html>
  <head>
    <title>SolYNaranjaS</title>
    <link rel="stylesheet" href="/styles/naranjas/naranjasStyle.css"/>
    <style type="text/css">
      {""".blah { background-image: url('/img/happyOranges.jpg'); }"""}
    </style>
  </head>
  <body>
    About SolYNaranjaS
    <a href="/something/about/oranges">CONCERT HERE</a>
    <img src="/img/sax.jpg"/>
    <img src="/img/internalbananasonly/mypic.jpg"/>
    <img src="/img/oranges.jpg"/>
    <img src="/img/bananas.jpg"/>
    <a href="/something/about/oranges">OR HERE</a>
  </body>
</html>

  val uniqueEncodedPathWithNumbers = Map(
    0 -> "/something/about/oranges",
    1 -> "/img/oranges.jpg"
  )

  val expectedUniquePaths = 6

  val expectedDoc =
"""<html>
  <head>
    <title>SolYNaranjaS</title>
    <link rel="stylesheet" href="/styles/naranjas/naranjasStyle.css"/>
    <style type="text/css">
      .blah { background-image: url('/img/happyOranges.jpg'); }
    </style>
  </head>
  <body>
    About SolYNaranjaS
    <a href="0">CONCERT HERE</a>
    <img src="/img/sax.jpg"/>
    <img src="/img/internalbananasonly/mypic.jpg"/>
    <img src="1"/>
    <img src="/img/bananas.jpg"/>
    <a href="2">OR HERE</a>
  </body>
</html>"""
}


