package wacc.semantic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.*
import wacc.ast.*
import wacc.q_ast.*
import wacc.renamer.*

class rename_prog_test extends AnyFlatSpec {
    "rename" should "be able to rename basic programs" in {
        val prog = Prog(
            List(),
            List(Skip.instance())
        )

        rename(prog) shouldBe (Q_Prog(
            List(),
            List(Q_Skip()),
            Set()
        ), TypeInfo(Map(), Map()))
    }

    it should "be able to rename programs with functions" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, "fun", List(), // int fun()
                    List(Return(IntLiteral(0)))  // return 0
                )
            ),
            List(Skip.instance())
        )

        rename(prog) shouldBe (Q_Prog(
            List(
                Q_Func(
                    BaseType.Int, Name("fun", 0), List(), // int fun/0()
                    List(Q_Return(Q_IntLiteral(0))),              // return 0  
                    Set()                                         // No locals
                )
            ),
            List(Q_Skip()),
            Set()
            ), TypeInfo(Map(), Map(Name("fun", 0) -> (KnownType.Int, List())))
        )
    }

    it should "be able to rename programs with functions with parameters" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, "fun", List(Param(BaseType.Int, "x")), // int fun(int x)
                    List(Return(Ident("x")))  // return x
                )
            ),
            List(Skip.instance())
        )

        rename(prog) shouldBe (Q_Prog(
            List(
                Q_Func(
                    BaseType.Int, Name("fun", 0), List(Q_Param(BaseType.Int, Name("x", 0))), // int fun/0(int x/0)
                    List(Q_Return(Q_Ident(Name("x", 0)))),              // return x/0
                    Set()                                         
                )
            ),
            List(Q_Skip()),
            Set()
            ), TypeInfo(Map(Name("x", 0) -> KnownType.Int), Map(Name("fun", 0) -> (KnownType.Int, List(Name("x", 0)))))
        )
    }
}
