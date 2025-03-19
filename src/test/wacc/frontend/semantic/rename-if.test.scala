package wacc.semantic

import wacc.ast.*
import wacc.error.ScopeException
import wacc.q_ast.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class rename_if_test extends AnyFlatSpec{
    "rename-if" should "be able to rename basic if statements" in {
        val prog = Prog(
            List(),
            List(
                If(
                    BoolLiteral(true),
                    List(
                        Skip.instance()
                    ),
                    List(
                        Skip.instance()
                    )
                )
            )
        )

        renamer.rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_If(
                    Q_BoolLiteral(true),
                    List(
                        Q_Skip()
                    ),
                    Set(),
                    List(
                        Q_Skip()
                    ),
                    Set()
                )
            ),
            Set()
        ),
        TypeInfo(
            Map(),
            Map()
        ))
    }
        
    it should "be able to rename if statements with variables" in {
        val prog = Prog(
            List(),
            List(
                If(
                    BoolLiteral(true),
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("x"),
                            IntLiteral(5)
                        )
                    ),
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

        renamer.rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_If(
                    Q_BoolLiteral(true),
                    List(
                        Q_Decl(
                            Name("x", 0),
                            Q_IntLiteral(5)
                        )
                    ),
                    Set(
                        Name("x", 0)
                    ),
                    List(
                        Q_Decl(
                            Name("y", 0),
                            Q_IntLiteral(6)
                        )
                    ),
                    Set(
                        Name("y", 0)   
                    )
                )
            ),
            Set()
        ), TypeInfo(
            Map(
                Name("x", 0) -> KnownType.Int,
                Name("y", 0) -> KnownType.Int
            ),
            Map()
        ))
    }

    it should "be able to shadow clashing variable names" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("x"),
                    IntLiteral(5)
                ),
                If(
                    BoolLiteral(true),
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("x"),
                            IntLiteral(6)
                        )
                    ),
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("x"),
                            IntLiteral(7)
                        )
                    )
                )
            )
        )

        renamer.rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_Decl(
                    Name("x", 0),
                    Q_IntLiteral(5)
                ),
                Q_If(
                    Q_BoolLiteral(true),
                    List(
                        Q_Decl(
                            Name("x", 1),
                            Q_IntLiteral(6)
                        )
                    ),
                    Set(
                        Name("x", 1)
                    ),
                    List(
                        Q_Decl(
                            Name("x", 2),
                            Q_IntLiteral(7)
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

    it should "fail to access a variable declared in 'if' clause from 'else' clause" in {
        a [ScopeException] should be thrownBy renamer.rename(
            Prog(
                List(),
                List(
                    If(
                        BoolLiteral(true),
                        List(
                            Decl(
                                BaseType.Int,
                                Ident("x"),
                                IntLiteral(5)
                            )
                        ),
                        List(
                            Asgn(
                                Ident("x"),
                                IntLiteral(6)
                            )
                        )
                    )
                )
            )
        )
    }

    it should "allow shadowed variables to have different types" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("x"),
                    IntLiteral(5)
                ),
                If(
                    BoolLiteral(true),
                    List(
                        Decl(
                            BaseType.String,
                            Ident("x"),
                            StringLiteral("hello")
                        )
                    ),
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("x"),
                            IntLiteral(6)
                        )
                    )
                )
            )
        )

        renamer.rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_Decl(
                    Name("x", 0),
                    Q_IntLiteral(5)
                ),
                Q_If(
                    Q_BoolLiteral(true),
                    List(
                        Q_Decl(
                            Name("x", 1),
                            Q_StringLiteral("hello")
                        )
                    ),
                    Set(
                        Name("x", 1)
                    ),
                    List(
                        Q_Decl(
                            Name("x", 2),
                            Q_IntLiteral(6)
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
                Name("x", 1) -> KnownType.String,
                Name("x", 2) -> KnownType.Int
            ),
            Map()
        ))
    }

    it should "be able to access variables from parent scope" in {
        val prog = Prog(
            List(),
            List(
                Decl(
                    BaseType.Int,
                    Ident("x"),
                    IntLiteral(5)
                ),
                If(
                    BoolLiteral(true),
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("y"),
                            Ident("x")
                        )
                    ),
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("z"),
                            Ident("x")
                        )
                    )
                )
            )
        )

        renamer.rename(prog) shouldBe (Q_Prog(
            List(),
            List(
                Q_Decl(
                    Name("x", 0),
                    Q_IntLiteral(5)
                ),
                Q_If(
                    Q_BoolLiteral(true),
                    List(
                        Q_Decl(
                            Name("y", 0),
                            Q_Ident(Name("x", 0))
                        )
                    ),
                    Set(
                        Name("y", 0)
                    ),
                    List(
                        Q_Decl(
                            Name("z", 0),
                            Q_Ident(Name("x", 0))
                        )
                    ),
                    Set(
                        Name("z", 0)
                    )
                )
            ),
            Set(
                Name("x", 0)
            )
        ), TypeInfo(
            Map(
                Name("x", 0) -> KnownType.Int,
                Name("y", 0) -> KnownType.Int,
                Name("z", 0) -> KnownType.Int
            ),
            Map()
        ))
    }
}
