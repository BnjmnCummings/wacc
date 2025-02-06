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
    "rename-decl" should "be able to rename basic programs" in {
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
                    Q_Name("x", 0),
                    Q_IntLiteral(5)
                )
            ),
            Set(
                Q_Name("x", 0)
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

        rename(prog) shouldBe Q_Prog(
            List(),
            List(
                Q_Decl(
                    BaseType.Int,
                    Q_Name("x", 0),
                    Q_IntLiteral(5)
                ),
                Q_Decl(
                    BaseType.Int,
                    Q_Name("y", 0),
                    Q_Ident(Q_Name("x", 0))
                )
            ),
            Set(
                Q_Name("x", 0),
                Q_Name("y", 0)
            )
        )
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
        rename(prog) shouldBe Q_Prog(
            List(),
            List(
                Q_Decl(
                    BaseType.Int,
                    Q_Name("x", 0),
                    Q_IntLiteral(6),
                ),
                Q_Asgn(
                    Q_Ident(Q_Name("x", 0)),
                    Q_Ident(Q_Name("x", 0)),
                ),
            ),
            Set(
                Q_Name("x", 0)
            )
        )
    }

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

        rename(prog) shouldBe Q_Prog(
            List(),
            List(
                Q_CodeBlock(
                    List(
                        Q_Decl(
                            BaseType.Int,
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
        )
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

        rename(prog) shouldBe Q_Prog(
            List(),
            List(
                Q_Decl(
                    BaseType.Int,
                    Q_Name("y", 0),
                    Q_IntLiteral(6)
                ),
                Q_CodeBlock(
                    List(
                        Q_Decl(
                            BaseType.Int,
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
                Q_Name("y", 0),
            )
        )
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

        rename(prog) shouldBe Q_Prog(
            List(),
            List(
                Q_Decl(
                    BaseType.Int,
                    Q_Name("y", 0),
                    Q_IntLiteral(6)
                ),
                Q_CodeBlock(
                    List(
                        Q_Decl(
                            BaseType.Int,
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
        )
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

        rename(prog) shouldBe Q_Prog(
            List(),
            List(
                Q_Decl(
                    BaseType.Int,
                    Q_Name("y", 0),
                    Q_IntLiteral(6)
                ),
                Q_CodeBlock(
                    List(
                        Q_Decl(
                            BaseType.Int,
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
        )
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

        rename(prog) shouldBe Q_Prog(
            List(),
            List(
                Q_Decl(
                    BaseType.Int,
                    Q_Name("y", 0),
                    Q_IntLiteral(6)
                ),
                Q_CodeBlock(
                    List(
                        Q_Decl(
                            BaseType.String,
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
        )
    }

    "rename-if" should "be able to rename basic if statements" in {
        val prog = Prog(
            List(),
            List(
                If(
                    BoolLiteral(true),
                    List(
                        Skip
                    ),
                    List(
                        Skip
                    )
                )
            )
        )

        rename(prog) shouldBe Q_Prog(
            List(),
            List(
                Q_If(
                    Q_BoolLiteral(true),
                    List(
                        Q_Skip
                    ),
                    Set(),
                    List(
                        Q_Skip
                    ),
                    Set()
                )
            ),
            Set()
        )
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

        rename(prog) shouldBe Q_Prog(
            List(),
            List(
                Q_If(
                    Q_BoolLiteral(true),
                    List(
                        Q_Decl(
                            BaseType.Int,
                            Q_Name("x", 0),
                            Q_IntLiteral(5)
                        )
                    ),
                    Set(
                        Q_Name("x", 0)
                    ),
                    List(
                        Q_Decl(
                            BaseType.Int,
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
        )
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

        rename(prog) shouldBe Q_Prog(
            List(),
            List(
                Q_Decl(
                    BaseType.Int,
                    Q_Name("x", 0),
                    Q_IntLiteral(5)
                ),
                Q_If(
                    Q_BoolLiteral(true),
                    List(
                        Q_Decl(
                            BaseType.Int,
                            Q_Name("x", 1),
                            Q_IntLiteral(6)
                        )
                    ),
                    Set(
                        Q_Name("x", 1)
                    ),
                    List(
                        Q_Decl(
                            BaseType.Int,
                            Q_Name("x", 2),
                            Q_IntLiteral(7)
                        )
                    ),
                    Set(
                        Q_Name("x", 2)
                    )
                )
            ),
            Set(
                Q_Name("x", 0)
            )
        )
    }

    it should "fail to access a variable declared in 'if' clause from 'else' clause" in {
        a [ScopeException] should be thrownBy rename(
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

        rename(prog) shouldBe Q_Prog(
            List(),
            List(
                Q_Decl(
                    BaseType.Int,
                    Q_Name("x", 0),
                    Q_IntLiteral(5)
                ),
                Q_If(
                    Q_BoolLiteral(true),
                    List(
                        Q_Decl(
                            BaseType.String,
                            Q_Name("x", 1),
                            Q_StringLiteral("hello")
                        )
                    ),
                    Set(
                        Q_Name("x", 1)
                    ),
                    List(
                        Q_Decl(
                            BaseType.Int,
                            Q_Name("x", 2),
                            Q_IntLiteral(6)
                        )
                    ),
                    Set(
                        Q_Name("x", 2)
                    )
                )
            ),
            Set(
                Q_Name("x", 0)
            )
        )
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

        rename(prog) shouldBe Q_Prog(
            List(),
            List(
                Q_Decl(
                    BaseType.Int,
                    Q_Name("x", 0),
                    Q_IntLiteral(5)
                ),
                Q_If(
                    Q_BoolLiteral(true),
                    List(
                        Q_Decl(
                            BaseType.Int,
                            Q_Name("y", 0),
                            Q_Ident(Q_Name("x", 0))
                        )
                    ),
                    Set(
                        Q_Name("y", 0)
                    ),
                    List(
                        Q_Decl(
                            BaseType.Int,
                            Q_Name("z", 0),
                            Q_Ident(Q_Name("x", 0))
                        )
                    ),
                    Set(
                        Q_Name("z", 0)
                    )
                )
            ),
            Set(
                Q_Name("x", 0)
            )
        )
    }

    "rename-call-statement" should "be able to shadow the name of a function" in {
        
    }
}

