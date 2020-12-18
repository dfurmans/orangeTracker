package parser.adt.interpreters

case class PathTrackerResult(trackedDoc: String,
                             encodedPaths: Map[Int, String],
                             uniquePaths: Int,
                             totalPaths: Int)
