package parser.generators

// Generic type for the ID State representation
trait Id[T,B] {
  def generateNextId: (T=>T) => B => (T, Id[T,B], Map[B,T])
}

object IdGenerators {

  val intervalIdNumber: Int = 1
  val generatorFunction : Int => Int = x => x + intervalIdNumber

  case class IntegerIdGenerator(aValue:Int, alreadyGenerated: Map[String,Int]) extends Id[Int,String] {
    override def generateNextId: (Int => Int) => String => (Int, Id[Int, String], Map[String, Int]) =
      keyFunctionGenerator => attributeKey => {
        (
          this.aValue,
          IntegerIdGenerator(aValue = keyFunctionGenerator(this.aValue), this.alreadyGenerated++Map(attributeKey -> this.aValue) ),
          this.alreadyGenerated
        )
      }
  }

}