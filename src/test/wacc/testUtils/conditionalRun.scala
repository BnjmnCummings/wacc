package wacc.testUtils

import org.scalatest.flatspec.AnyFlatSpec

trait ConditionalRun extends AnyFlatSpec{
    def runIfTrue(
        testSettings: scala.collection.mutable.HashMap[String, Boolean],
        test: String,
        testBody: () => Unit
    ): Unit = {
        if (testSettings.getOrElse(test, false)) {
            testBody()
        } else {
            it should s"pass $test" in {
                pending
            }
        }
    }
}
