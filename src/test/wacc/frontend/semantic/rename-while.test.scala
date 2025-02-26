package wacc.semantic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.ast.*
import wacc.q_ast.*
import wacc.renamer.*
import wacc.*

class rename_while_test extends AnyFlatSpec{
  "rename-while" should "be able to rename basic codeblocks and scopes should be confined" in {
        val prog = Prog(
            List(),
            List(
               While(
                    BoolLiteral(true), 
                    List(Skip.instance())
                )
            )
        )

        rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_While(
                    Q_BoolLiteral(true),
                    List(
                        Q_Skip()
                    ),
                    Set()
                )
            ),
            Set()
        ), TypeInfo(
            Map(),
            Map()
        ))
    }

    it should "be able to declare variables in while scope" in {
        val prog = Prog(
            List(),
            List(
                While(
                    BoolLiteral(true),
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("x"),
                            IntLiteral(5)
                        )
                    )
                )
            )
        )

        rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_While(
                    Q_BoolLiteral(true),
                    List(
                        Q_Decl(
                            Name("x", 0),
                            Q_IntLiteral(5)
                        )
                    ),
                    Set(
                        Name("x", 0)
                    )
                )
            ),
            Set()
        ), TypeInfo(
            Map(
                Name("x", 0) -> KnownType.Int
            ),
            Map()
        ))
    }

    it should "be able to rename clashing while scopes" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("x"),
                    IntLiteral(5)
                ),
                While(
                    BoolLiteral(true),
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("x"),
                            IntLiteral(10)
                        )
                    )
                ),
                While(
                    BoolLiteral(true),
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("x"),
                            IntLiteral(15)
                        )
                    )
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
                Q_While(
                    Q_BoolLiteral(true),
                    List(
                        Q_Decl(
                            Name("x", 1),
                            Q_IntLiteral(10)
                        )
                    ),
                    Set(
                        Name("x", 1)
                    )
                ),
                Q_While(
                    Q_BoolLiteral(true),
                    List(
                        Q_Decl(
                            Name("x", 2),
                            Q_IntLiteral(15)
                        )
                    ),
                    Set(
                        Name("x", 2)
                    )
                )
            ),
            Set(
                Name("x", 0)
            )
        ), TypeInfo(
            Map(
                Name("x", 0) -> KnownType.Int,
                Name("x", 1) -> KnownType.Int,
                Name("x", 2) -> KnownType.Int
            ),
            Map()
        ))
    }

    it should "be able to access variables from a parent scope" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("x"),
                    IntLiteral(5)
                ),
                While(
                    BoolLiteral(true),
                    List(
                        Asgn(
                            Ident("x"),
                            IntLiteral(5)
                        )
                    )
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
            Q_While(
                Q_BoolLiteral(true),
                List(
                    Q_Asgn(
                        Q_Ident(Name("x", 0)),
                        Q_IntLiteral(5)
                    )
                ),
                Set()
            )
            ),
            Set(
            Name("x", 0)
            )
        ), TypeInfo(
            Map(
            Name("x", 0) -> KnownType.Int
            ),
            Map()
        ))
    }

    it should "be able to rename nested while scopes" in {
        val prog = Prog(
            List(),
            List(
                While(
                    BoolLiteral(true),
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("x"),
                            IntLiteral(10)
                        ),
                        While(
                            BoolLiteral(true),
                            List(
                                Asgn(
                                    Ident("x"),
                                    IntLiteral(5)
                                )
                            )
                        )
                    )
                )
            )
        )

        rename(prog) shouldBe (Q_Prog(
            List(),
            List(
            Q_While(
                Q_BoolLiteral(true),
                List(
                Q_Decl(
                    Name("x", 0),
                    Q_IntLiteral(10)
                ),
                Q_While(
                    Q_BoolLiteral(true),
                    List(
                    Q_Asgn(
                        Q_Ident(Name("x", 0)),
                        Q_IntLiteral(5)
                    )
                    ),
                    Set()
                )
                ),
                Set(Name("x", 0))
            )
            ),
            Set()
        ), TypeInfo(
            Map(
            Name("x", 0) -> KnownType.Int
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
                        Ident("x"),
                        IntLiteral(5)
                    ),
                    While(
                        BoolLiteral(true),
                        List(
                            Decl(
                                BaseType.Int,
                                Ident("x"),
                                IntLiteral(10)
                            ),
                            Asgn(
                                Ident("x"),
                                IntLiteral(5)
                            )
                        )
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
                    Q_While(
                        Q_BoolLiteral(true),
                        List(
                            Q_Decl(
                                Name("x", 1),
                                Q_IntLiteral(10)
                            ),
                            Q_Asgn(
                                Q_Ident(Name("x", 1)),
                                Q_IntLiteral(5)
                            )
                        ),
                        Set(
                            Name("x", 1)
                        )
                    )
                ),
                Set(
                    Name("x", 0)
                )
            ), TypeInfo(
                Map(
                    Name("x", 0) -> KnownType.Int,
                    Name("x", 1) -> KnownType.Int
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
                        Ident("x"),
                        IntLiteral(5)
                    ),
                    While(
                        BoolLiteral(true),
                        List(
                            Decl(
                                BaseType.String,
                                Ident("x"),
                                StringLiteral("string")
                            ),
                            Asgn(
                                Ident("x"),
                                StringLiteral("newString")
                            )
                        )
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
                    Q_While(
                        Q_BoolLiteral(true),
                        List(
                            Q_Decl(
                                Name("x", 1),
                                Q_StringLiteral("string")
                            ),
                            Q_Asgn(
                                Q_Ident(Name("x", 1)),
                                Q_StringLiteral("newString")
                            )
                        ),
                        Set(
                            Name("x", 1)
                        )
                    )
                ),
                Set(
                    Name("x", 0)
                )
            ), TypeInfo(
                Map(
                    Name("x", 0) -> KnownType.Int,
                    Name("x", 1) -> KnownType.String
                ),
                Map()
            ))
    }

    it should "be able to access parent scope variables in the condition" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("x"),
                    IntLiteral(5)
                ),
                While(
                    Ident("x"),
                    List(
                        Asgn(
                            Ident("x"),
                            IntLiteral(10)
                        )
                    )
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
                Q_While(
                    Q_Ident(Name("x", 0)),
                    List(
                        Q_Asgn(
                            Q_Ident(Name("x", 0)),
                            Q_IntLiteral(10)
                        )
                    ),
                    Set()
                )
            ),
            Set(
                Name("x", 0)
            )
        ), TypeInfo(
            Map(
                Name("x", 0) -> KnownType.Int
            ),
            Map()
        ))
    }

    it should "be able to rename variables in nested while loop conditions" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("x"),
                    IntLiteral(10)
                ),
                While(
                    BoolLiteral(true),
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("x"),
                            IntLiteral(5)
                        ),
                        While(
                            Ident("x"),
                            List(
                                Asgn(
                                    Ident("x"),
                                    IntLiteral(5)
                                )
                            )
                        )
                    )
                )
            )
        )

        rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_Decl(
                    Name("x", 0),
                    Q_IntLiteral(10)
                ),
                Q_While(
                    Q_BoolLiteral(true),
                    List(
                        Q_Decl(
                            Name("x", 1),
                            Q_IntLiteral(5)
                        ),
                        Q_While(
                            Q_Ident(Name("x", 1)),
                            List(
                                Q_Asgn(
                                    Q_Ident(Name("x", 1)),
                                    Q_IntLiteral(5)
                                )
                            ),
                            Set()
                        )
                    ),
                    Set(Name("x", 1))
                )
            ),
            Set(
                Name("x", 0)
            )
        ), TypeInfo(
            Map(
                Name("x", 0) -> KnownType.Int,
                Name("x", 1) -> KnownType.Int
            ),
            Map()
        ))
    }
}
