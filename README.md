# A Simple HTML/XML Parser

## Overview

1. For a given HTML/XML document as String
* Creates a proper type representation

Support a simple document transformation based on given transformations rules.

The flow could be showed as

```txt

XML/HTML as String => XmlType => TransformDocumentRules => XmlType => Print as String

```
## The current configuration is set to change the word "Oranges" into a next Int numbers  

```scala
  val defaultTransformationRules = TransformDocumentRules(
  elements = SearchEngine.elementToSearchFor,
  attributes = SearchEngine.attributesToSearchFor,
  valuesIncluded = SearchEngine.defaultReqAttributeChunks.chunksAttributeNamesIncluded,
  valuesExcluded = SearchEngine.defaultReqAttributeChunks.chunksAttributeNameExcluded
)

```
