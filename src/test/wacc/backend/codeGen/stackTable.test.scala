package wacc.codeGen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.q_ast.Name
import wacc.TypeInfo
import wacc.KnownType

class stack_table_test extends AnyFlatSpec with Matchers {

    "StackTable" should "be ablt to add variables to the table via addScope" in {
        val stackTable = StackTable(0)
        val typeInfo = TypeInfo(
            Map(Name("x", 0) -> KnownType.Int, Name("y", 0) -> KnownType.Int),
            Map()
        )
        stackTable.addScope(Set(Name("x", 0), Name("y", 0)), typeInfo)

        stackTable.contains(Name("x", 0)) shouldBe (true)
        stackTable.contains(Name("y", 0)) shouldBe (true)
    }

    it should "return correct offset for variables" in {
        val stackTable = StackTable(0)
        val typeInfo = TypeInfo(
            Map(Name("x", 0) -> KnownType.Int, Name("y", 0) -> KnownType.Int),
            Map()
        )
        stackTable.addScope(Set(Name("x", 0), Name("y", 0)), typeInfo)

        stackTable(Name("x", 0)) shouldBe (4)
        stackTable(Name("y", 0)) shouldBe (8)
    }

    it should "throw a runtime exception when quereying offsets for variables not in scope" in {
        val stackTable = StackTable(0)
        val typeInfo = TypeInfo(
            Map(Name("x", 0) -> KnownType.Int, Name("y", 0) -> KnownType.Int),
            Map()
        )
        stackTable.addScope(Set(), typeInfo)

        a [RuntimeException] should be thrownBy stackTable(Name("x", 0))
    }

    it should "offset all the variables by <basePtrOffset>" in {
        val stackTable = StackTable(4)
        val typeInfo = TypeInfo(
            Map(Name("x", 0) -> KnownType.Int, Name("y", 0) -> KnownType.Int),
            Map()
        )
        stackTable.addScope(Set(Name("x", 0), Name("y", 0)), typeInfo)

        stackTable(Name("x", 0)) shouldBe (8)
        stackTable(Name("y", 0)) shouldBe (12)
    }

    it should "be able to add shadowed variables to the table" in {
        val stackTable = StackTable(0)
        val typeInfo = TypeInfo(
            Map(Name("y", 0) -> KnownType.Int, Name("y", 1) -> KnownType.Int),
            Map()
        )
        stackTable.addScope(Set(Name("y", 0)), typeInfo)
        stackTable.addScope(Set(Name("y", 1)), typeInfo)

        stackTable(Name("y", 0)) shouldBe 4
        stackTable(Name("y", 1)) shouldBe 8
    }

    "StackTables" should "be able to add scopes correctly" in {
        val stackTables = StackTables(None)
        val typeInfo = TypeInfo(
            Map(Name("x", 0) -> KnownType.Int, Name("y", 0) -> KnownType.Int),
            Map()
        )
        stackTables.addScope(Set(Name("x", 0), Name("y", 0)), typeInfo)

        stackTables.scopeSize shouldBe (8)
    }

    it should "be able to get variable from current scope" in {
        given CodeGenCtx = CodeGenCtx(
            TypeInfo(
                Map(Name("x", 0) -> KnownType.Int), 
                Map()
            ),
            TableCtx(
                StackTables(None)
            )
        )

        val stackTables = StackTables(None)
        stackTables.addScope(Set(Name("x", 0)), summon[CodeGenCtx].typeInfo)

        val instrs = stackTables.get(Name("x", 0))
        instrs.nonEmpty shouldBe (true)
    }

    it should "throw exception if variable not found" in {
        given CodeGenCtx = CodeGenCtx(
            TypeInfo(
                Map(Name("x", 0) -> KnownType.Int), 
                Map()
            ),
            TableCtx(
                StackTables(None)
            )
        )

        val stackTables = StackTables(None)

        a [RuntimeException] shouldBe thrownBy { stackTables.get(Name("y", 0)) }
    }

    it should "be ablt to set variable in current scope" in {
        given CodeGenCtx = CodeGenCtx(
            TypeInfo(
                Map(Name("x", 0) -> KnownType.Int), 
                Map()
            ),
            TableCtx(
                StackTables(None)
            )
        )
        val stackTables = StackTables(None)
        stackTables.addScope(Set(Name("x", 0)), summon[CodeGenCtx].typeInfo)

        val instrs = stackTables.set(Name("x", 0))
        instrs.nonEmpty shouldBe (true)
    }

    it should "throw exception if setting variable not found" in {
        given CodeGenCtx = CodeGenCtx(
            TypeInfo(
                Map(Name("x", 0) -> KnownType.Int), 
                Map()
            ),
            TableCtx(
                StackTables(None)
            )
        )
        val stackTables = StackTables(None)

        a [RuntimeException] shouldBe thrownBy { stackTables.set(Name("y", 0)) }
    }

    it should "be able to generate argument store instructions" in {
        given CodeGenCtx = CodeGenCtx(
            TypeInfo(
                Map(Name("x", 0) -> KnownType.Int, Name("y", 0) -> KnownType.Int), 
                Map()
            ),
            TableCtx(
                StackTables(None)
            )
        )
        val paramTable = StackTable(0)
        paramTable.addScope(Set(Name("x", 0), Name("y", 0)), summon[CodeGenCtx].typeInfo)
        val stackTables = StackTables(Some(paramTable))

        val instrs = stackTables.argStoreInstrs(List(Name("x", 0), Name("y", 0)))
        instrs.size shouldBe (2)
    }
}
