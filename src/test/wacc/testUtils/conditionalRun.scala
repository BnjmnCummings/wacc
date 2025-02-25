package wacc.testUtils

import org.scalatest.flatspec.AnyFlatSpec

trait ConditionalRun extends AnyFlatSpec{
    def runIfTrue(
        testSettings: scala.collection.mutable.HashMap[String, Boolean],
        test: String,
        testFunction: () => Any
    ): Unit = {
        it should test in {
            if (testSettings.getOrElse(test, false)) {
                testFunction
            } else {
                pending
            }
        }
    }
}
