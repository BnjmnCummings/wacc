package test.wacc.semantic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.ast.*
import wacc.q_ast.*
import wacc.renamer.*
import java.lang.foreign.MemorySegment.Scope
import wacc.ScopeException

class rename_stmt_test extends AnyFlatSpec {
    /* control */
    "rename (stmt)" should "be able to rename basic programs" in {
        val prog = Prog(
            List(),
            List(Skip)
        )

        rename(prog) shouldBe Q_Prog(
            List(),
            List(Q_Skip),
            Set()
        )
    }

    /* Declaration and Assignment */
    it should "be able to rename basic declarations" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("x"),
                    IntLiteral(5)
                )
            )
        )

        rename(prog) shouldBe Q_Prog(
            List(),
            List(
                Q_Decl(
                    BaseType.Int,
                    Q_Name("x", "x/0"),
                    Q_IntLiteral(5)
                )
            ),
            Set(
                Q_Name("x", "x/0")
            )
        )
    }

    it should "fail duplicate declarations" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("x"),
                    IntLiteral(5)
                ),
                Decl(
                    BaseType.Int,
                    Ident("x"),
                    IntLiteral(6)
                ),
            )
        )

        a [ScopeException] should be thrownBy rename(prog)
    }

    it should "fail duplicate declarations regardless of type" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("x"),
                    IntLiteral(5)
                ),
                Decl(
                    BaseType.String,
                    Ident("x"),
                    StringLiteral("hello")
                ),
            )
        )

        a [ScopeException] should be thrownBy rename(prog)
    }

    it should "fail to retrieve a vairable that hasn't been declared" in {
        a [ScopeException] should be thrownBy rename(
            Prog(
                List(),
                List(
                    Asgn(
                        Ident("x"),
                        IntLiteral(5)
                    ),
                    Decl(
                        BaseType.Int,
                        Ident("x"),
                        IntLiteral(5)
                    ),
                )
            )
        )

        a [ScopeException] should be thrownBy rename(
            Prog(
                List(),
                List(
                    Decl(
                        BaseType.Int,
                        Ident("y"),
                        Ident("x")
                    ),
                    Decl(
                        BaseType.Int,
                        Ident("x"),
                        IntLiteral(5)
                    ),
                )
            )
        )
    }

    it should "be able to use in-scope identities declarations" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("x"),
                    IntLiteral(5)
                ),
                Decl(
                    BaseType.Int,
                    Ident("y"),
                    Ident("x")
                )
            )
        )

        rename(prog) shouldBe Q_Prog(
            List(),
            List(
                Q_Decl(
                    BaseType.Int,
                    Q_Name("x", "x/0"),
                    Q_IntLiteral(5)
                ),
                Q_Decl(
                    BaseType.Int,
                    Q_Name("y", "y/0"),
                    Q_Ident(Q_Name("x", "x/0"))
                )
            ),
            Set(
                Q_Name("x", "x/0"),
                Q_Name("y", "y/0")
            )
        )
    }

    it should "fail to declare a variable to itself" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("x"),
                    Ident("x"),
                ),
            )
        )

        a [ScopeException] should be thrownBy rename(prog)
    }

    it should "be able to assign a variable to itself" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("x"),
                    IntLiteral(6),
                ),
                Asgn(
                    Ident("x"),
                    Ident("x"),
                ),
            )
        )
        rename(prog) shouldBe Q_Prog(
            List(),
            List(
                Q_Decl(
                    BaseType.Int,
                    Q_Name("x", "x/0"),
                    Q_IntLiteral(6),
                ),
                Q_Asgn(
                    Q_Ident(Q_Name("x", "x/0")),
                    Q_Ident(Q_Name("x", "x/0")),
                ),
            ),
            Set(
                Q_Name("x", "x/0")
            )
        )
    }
}
