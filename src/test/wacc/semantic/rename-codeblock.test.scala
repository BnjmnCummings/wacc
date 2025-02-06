package wacc.semantic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.ast.*
import wacc.q_ast.*
import wacc.renamer.*
import wacc.ScopeException
import wacc.*

class rename_codeblock_test extends AnyFlatSpec{
  "rename-codeblocks" should "be able to rename basic codeblocks and scopes should be confined" in {
        val prog = Prog(
            List(),
            List(
                CodeBlock(
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("y"),
                            IntLiteral(6)
                        )
                    )
                )
            )
        )

        rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_CodeBlock(
                    List(
                        Q_Decl(
                            Q_Name("y", 0),
                            Q_IntLiteral(6)
                        )
                    ),
                    Set(
                        Q_Name("y", 0)
                    )
                )
            ),
            Set()
        ), TypeInfo(
            Map(
                Q_Name("y", 0) -> KnownType.Int
            ),
            Map()
        ))
    }

    it should "be able to rename clashing codeblock scopes" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("y"),
                    IntLiteral(6)
                ),
                CodeBlock(
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("y"),
                            IntLiteral(6)
                        )
                    )
                )
            )
        )

        rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_Decl(
                    Q_Name("y", 0),
                    Q_IntLiteral(6)
                ),
                Q_CodeBlock(
                    List(
                        Q_Decl(
                            Q_Name("y", 1),
                            Q_IntLiteral(6)
                        )
                    ),
                    Set(
                        Q_Name("y", 1)
                    )
                )
            ),
            Set(
                Q_Name("y", 0)
            )
        ), TypeInfo(
            Map(
                Q_Name("y", 0) -> KnownType.Int,
                Q_Name("y", 1) -> KnownType.Int
            ),
            Map()
        ))
    }

    it should "be able to access variables in a parent scope" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("y"),
                    IntLiteral(6)
                ),
                CodeBlock(
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("x"),
                            Ident("y")
                        )
                    )
                )
            )
        )

        rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_Decl(
                    Q_Name("y", 0),
                    Q_IntLiteral(6)
                ),
                Q_CodeBlock(
                    List(
                        Q_Decl(
                            Q_Name("x", 0),
                            Q_Ident(Q_Name("y", 0))
                        )
                    ),
                    Set(
                        Q_Name("x", 0)
                    )
                )
            ),
            Set(
                Q_Name("y", 0)
            )
        ), TypeInfo(
            Map(
                Q_Name("y", 0) -> KnownType.Int,
                Q_Name("x", 0) -> KnownType.Int
            ),
            Map()
        ))
    }

    it should "let a shadowed variable be set to the parent variable" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("y"),
                    IntLiteral(6)
                ),
                CodeBlock(
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("y"),
                            Ident("y")
                        )
                    )
                )
            )
        )

        rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_Decl(
                    Q_Name("y", 0),
                    Q_IntLiteral(6)
                ),
                Q_CodeBlock(
                    List(
                        Q_Decl(
                            Q_Name("y", 1),
                            Q_Ident(Q_Name("y", 0))
                        )
                    ),
                    Set(
                        Q_Name("y", 1)
                    )
                )
            ),
            Set(
                Q_Name("y", 0)
            )
        ), TypeInfo(
            Map(
                Q_Name("y", 0) -> KnownType.Int,
                Q_Name("y", 1) -> KnownType.Int
            ),
            Map()
        ))
    }

    it should "let shadowed variables have different types" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("y"),
                    IntLiteral(6)
                ),
                CodeBlock(
                    List(
                        Decl(
                            BaseType.String,
                            Ident("y"),
                            StringLiteral("hello")
                        )
                    )
                )
            )
        )

        rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_Decl(
                    Q_Name("y", 0),
                    Q_IntLiteral(6)
                ),
                Q_CodeBlock(
                    List(
                        Q_Decl(
                            Q_Name("y", 1),
                            Q_StringLiteral("hello")
                        )
                    ),
                    Set(
                        Q_Name("y", 1)
                    )
                )
            ),
            Set(
                Q_Name("y", 0)
            )
        ), TypeInfo(
            Map(
                Q_Name("y", 0) -> KnownType.Int,
                Q_Name("y", 1) -> KnownType.String
            ),
            Map()
        ))
    }
}
