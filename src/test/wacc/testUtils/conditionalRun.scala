package wacc.testUtils

import org.scalatest.flatspec.AnyFlatSpec

trait ConditionalRun extends AnyFlatSpec{
    def runIfTrue(
        testSettings: scala.collection.immutable.Set[String],
        test: String,
        testBody: () => Unit
    ): Unit = {
        if (testSettings.contains(test)) {
            testBody()
        } else {
            it should s"pass $test" in {
                pending
            }
        }
    }
}
