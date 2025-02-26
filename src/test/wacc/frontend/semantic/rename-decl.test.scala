package wacc.semantic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.*
import wacc.ast.*
import wacc.q_ast.*
import wacc.renamer.*
import wacc.ScopeException

class rename_decl_test extends AnyFlatSpec {
    /* control */
    "rename-decl" should "be able to rename basic programs" in {
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

        rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_Decl(
                    Name("x", 0),
                    Q_IntLiteral(5)
                )
            ),
            Set(
                Name("x", 0)
            )
        ), TypeInfo(
            Map(Name("x", 0) -> KnownType.Int),
            Map()
        ))
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

    // it should "fail to change the type of a variable in scope" in {
    //     val prog = Prog(
    //         List(),
    //         List(
    //             Decl(
    //                 BaseType.Int,
    //                 Ident("x"),
    //                 IntLiteral(5)
    //             ),
    //             Asgn(
    //                 Ident("x"),
    //                 StringLiteral("string")
    //             )
    //         )
    //     )

    //     a [ScopeException] should be thrownBy rename(prog)
    // }

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

        rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_Decl(
                    Name("x", 0),
                    Q_IntLiteral(5)
                ),
                Q_Decl(
                    Name("y", 0),
                    Q_Ident(Name("x", 0))
                )
            ),
            Set(
                Name("x", 0),
                Name("y", 0)
            )
        ), TypeInfo(
            Map(
                Name("x", 0) -> KnownType.Int,
                Name("y", 0) -> KnownType.Int
            ),
            Map()
        ))
    }

    it should "fail to declare a variable to itself" in {
        a [ScopeException] should be thrownBy rename(
            Prog(
                List(),
                List(
                    Decl(
                        BaseType.Int,
                        Ident("x"),
                        Ident("x"),
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
                        Ident("x"),
                        Ident("x + 7"),
                    ),
                )
            )
        )
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
        rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_Decl(
                    Name("x", 0),
                    Q_IntLiteral(6),
                ),
                Q_Asgn(
                    Q_Ident(Name("x", 0)),
                    Q_Ident(Name("x", 0)),
                ),
            ),
            Set(
                Name("x", 0)
            )
        ), TypeInfo(
            Map(
                Name("x", 0) -> KnownType.Int,
            ),
            Map()
        ))
    }
}

