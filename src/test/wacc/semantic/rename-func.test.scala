package test.wacc.semantic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.ast.*
import wacc.q_ast.*
import wacc.renamer.*
import wacc.ScopeException
import wacc.*


class rename_func_test extends AnyFlatSpec {

    /* control test */
    "rename-func" should "be able to rename programs with functions" in {
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
                    BaseType.Int, Q_Name("fun", 0), List(), // int fun/0()
                    List(Q_Return(Q_IntLiteral(0))),              // return 0  
                    Set()                                         // No locals
                )
            ),
            List(Q_Skip()),
            Set(
                Q_Name("fun", 0)
            )
        ), TypeInfo(Map(), Map(Q_Name("fun", 0) -> (KnownType.Int, List()))))
    }

    it should "be able to rename functions with parameters" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, 
                    "fun", 
                    List(Param(BaseType.Int, "aryaNarang")),
                    List(Return(IntLiteral(0)))
                )
            ),
            List(Skip.instance())
        )

        rename(prog) shouldBe (Q_Prog(
            List(
                Q_Func(
                    BaseType.Int, 
                    Q_Name("fun", 0), 
                    List(Q_Param(BaseType.Int, Q_Name("aryaNarang", 0))), 
                    List(Q_Return(Q_IntLiteral(0))),              
                    Set()                                      
                )
            ),
            List(Q_Skip()),
            Set(
                Q_Name("fun", 0)
            )
        ), TypeInfo(Map(Q_Name("aryaNarang", 0) -> KnownType.Int), Map(Q_Name("fun", 0) -> (KnownType.Int, List(Q_Name("aryaNarang", 0))))))
    }

    it should "be able to raname many functions without clashing scopes" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, 
                    "fun", 
                    List(Param(BaseType.Int, "aryaNarang")), 
                    List(Return(IntLiteral(0)))  
                ),
                Func(
                    BaseType.Int, 
                    "fun2", 
                    List(Param(BaseType.Int, "tejasMungale")),
                    List(Return(IntLiteral(0)))
                )
            ),
            List(Skip.instance())
        )

        rename(prog) shouldBe (Q_Prog(
            List(
                Q_Func(
                    BaseType.Int, 
                    Q_Name("fun", 0), 
                    List(Q_Param(BaseType.Int, Q_Name("aryaNarang", 0))), 
                    List(Q_Return(Q_IntLiteral(0))),              
                    Set()                                      
                ),

                Q_Func(
                    BaseType.Int, 
                    Q_Name("fun2", 0), 
                    List(Q_Param(BaseType.Int, Q_Name("tejasMungale", 0))), 
                    List(Q_Return(Q_IntLiteral(0))),              
                    Set()                                      
                )
            ),
            List(Q_Skip()),
            Set(
                Q_Name("fun2", 0),
                Q_Name("fun", 0),
               
            )
        ), TypeInfo(Map(Q_Name("aryaNarang", 0) -> KnownType.Int, Q_Name("tejasMungale", 0) -> KnownType.Int), Map(Q_Name("fun", 0) -> (KnownType.Int, List(Q_Name("aryaNarang", 0))), Q_Name("fun2", 0) -> (KnownType.Int, List(Q_Name("tejasMungale", 0)))))
        )
    }

    it should "be able to access parameters as a parent scope" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, 
                    "fun", 
                    List(Param(BaseType.Int, "aryaNarang")),
                    List(
                        Decl(
                            BaseType.Int,
                            Ident("x"),
                            Ident("aryaNarang"), 
                        ),
                        Return(IntLiteral(0))
                    )
                )
            ),
            List(Skip.instance())
        )

        rename(prog) shouldBe (Q_Prog(
            List(
                Q_Func(
                    BaseType.Int, 
                    Q_Name("fun", 0), 
                    List(Q_Param(BaseType.Int, Q_Name("aryaNarang", 0))), 
                    List(
                        Q_Decl(
                            Q_Name("x", 0),
                            Q_Ident(Q_Name("aryaNarang", 0)),
                        ),
                        Q_Return(Q_IntLiteral(0))
                    ),              
                    Set(Q_Name("x", 0))                                      
                )
            ),
            List(Q_Skip()),
            Set(
                Q_Name("fun", 0)
            )
        ), TypeInfo(Map(Q_Name("aryaNarang", 0) -> KnownType.Int, Q_Name("x", 0) -> KnownType.Int), Map(Q_Name("fun", 0) -> (KnownType.Int, List(Q_Name("aryaNarang", 0))))))
    }

        it should "be able to mutate parameters as a parent scope" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, 
                    "fun", 
                    List(Param(BaseType.Int, "aryaNarang")),
                    List(
                        Asgn(
                            Ident("aryaNarang"), 
                            IntLiteral(0)
                        ),
                        Return(IntLiteral(0))
                    )
                )
            ),
            List(Skip.instance())
        )

        rename(prog) shouldBe (Q_Prog(
            List(
                Q_Func(
                    BaseType.Int, 
                    Q_Name("fun", 0), 
                    List(Q_Param(BaseType.Int, Q_Name("aryaNarang", 0))), 
                    List(
                        Q_Asgn(
                            Q_Ident(Q_Name("aryaNarang", 0)),
                            Q_IntLiteral(0)
                        ),
                        Q_Return(Q_IntLiteral(0))
                    ),              
                    Set()                                      
                )
            ),
            List(Q_Skip()),
            Set(
                Q_Name("fun", 0)
            )
        ), TypeInfo(Map(Q_Name("aryaNarang", 0) -> KnownType.Int), Map(Q_Name("fun", 0) -> (KnownType.Int, List(Q_Name("aryaNarang", 0)))))
        )
    }

    it should "be able to rename declarations inside functions" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, 
                    "fun", 
                    List(),
                    List(
                        Decl(
                            BaseType.Int, 
                            Ident("myVar"), 
                            IntLiteral(0)
                        ),
                        Return(IntLiteral(0))
                    )
                )
            ),
            List(Skip.instance())
        )

        rename(prog) shouldBe (Q_Prog(
            List(
                Q_Func(
                    BaseType.Int, 
                    Q_Name("fun", 0), 
                    List(), 
                    List(
                        Q_Decl(
                            Q_Name("myVar", 0), 
                            Q_IntLiteral(0)
                        ),
                        Q_Return(Q_IntLiteral(0))
                    ),              
                    Set(Q_Name("myVar", 0))                                      
                )
            ),
            List(Q_Skip()),
            Set(
                Q_Name("fun", 0)
            )
        ), TypeInfo(Map(Q_Name("myVar", 0) -> KnownType.Int), Map(Q_Name("fun", 0) -> (KnownType.Int, List())))
        )
    }

    it should "be able to rename assignments inside functions" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, 
                    "fun", 
                    List(),
                    List(
                        Decl(
                            BaseType.Int, 
                            Ident("myVar"), 
                            IntLiteral(0)
                        ),
                        Asgn(
                            Ident("myVar"), 
                            IntLiteral(0)
                        ),
                        Return(IntLiteral(0))
                    )
                )
            ),
            List(Skip.instance())
        )

        rename(prog) shouldBe (Q_Prog(
            List(
                Q_Func(
                    BaseType.Int, 
                    Q_Name("fun", 0), 
                    List(), 
                    List(
                        Q_Decl(
                            Q_Name("myVar", 0), 
                            Q_IntLiteral(0)
                        ),
                        Q_Asgn(
                            Q_Ident(Q_Name("myVar", 0)), 
                            Q_IntLiteral(0)
                        ),
                        Q_Return(Q_IntLiteral(0))
                    ),              
                    Set(Q_Name("myVar", 0))                                      
                )
            ),
            List(Q_Skip()),
            Set(
                Q_Name("fun", 0)
            )
        ), TypeInfo(Map(Q_Name("myVar", 0) -> KnownType.Int), Map(Q_Name("fun", 0) -> (KnownType.Int, List())))
        )
    }


    /* currently dysfunctional */
    it should "fail assignments to variables that don't exist" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, 
                    "fun", 
                    List(),
                    List(
                        Asgn(
                            Ident("myVar"), 
                            IntLiteral(0)
                        ),
                        Return(IntLiteral(0))
                    )
                )
            ),
            List(Skip.instance())
        )

        a [ScopeException] should be thrownBy rename(prog)
    }

    it should "fail clashing variable names inside functions" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, 
                    "fun", 
                    List(),
                    List(
                        Decl(
                            BaseType.Int, 
                            Ident("myVar"), 
                            IntLiteral(0)
                        ),
                        Decl(
                            BaseType.Int, 
                            Ident("myVar"), 
                            IntLiteral(0)
                        ),
                        Asgn(
                            Ident("myVar"), 
                            IntLiteral(0)
                        ),
                        Return(IntLiteral(0))
                    )
                )
            ),
            List(Skip.instance())
        )

        a [ScopeException] should be thrownBy rename(prog)
    }

    it should "fail to access func variables from inside the program body" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, 
                    "fun", 
                    List(),
                    List(
                        Decl(
                            BaseType.Int, 
                            Ident("myVar"), 
                            IntLiteral(0)
                        ),
                        Return(IntLiteral(0))
                    )
                )
            ),
            List(
                Asgn(
                    Ident("myVar"), 
                    IntLiteral(0)
                )
            )
        )

        a [ScopeException] should be thrownBy rename(prog)
    }

    it should "be able to have a parameter with the same name as a function" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, 
                    "fun", 
                    List(Param(BaseType.Int, "fun")),
                    List(Return(IntLiteral(0)))
                )
            ),
            List(Skip.instance())
        )

        rename(prog) shouldBe (Q_Prog(
            List(
                Q_Func(
                    BaseType.Int, 
                    Q_Name("fun", 0), 
                    List(Q_Param(BaseType.Int, Q_Name("fun", 1))), 
                    List(Q_Return(Q_IntLiteral(0))),              
                    Set()                                      
                )
            ),
            List(Q_Skip()),
            Set(
                Q_Name("fun", 0)
            )
        ), TypeInfo(Map(Q_Name("fun", 1) -> KnownType.Int), Map(Q_Name("fun", 0) -> (KnownType.Int, List(Q_Name("fun", 1)))))
        )
    }

    it should "be able to shadow parameters" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, 
                    "fun", 
                    List(Param(BaseType.Int, "param")),
                    List(
                        Decl(
                            BaseType.Int, 
                            Ident("param"), 
                            IntLiteral(0)
                        ),
                        Return(IntLiteral(0))
                    )
                )
            ),
            List(Skip.instance())
        )

        rename(prog) shouldBe (Q_Prog(
            List(
            Q_Func(
                BaseType.Int, 
                Q_Name("fun", 0), 
                List(Q_Param(BaseType.Int, Q_Name("param", 0))), 
                List(
                Q_Decl(
                    Q_Name("param", 1), 
                    Q_IntLiteral(0)
                ),
                Q_Return(Q_IntLiteral(0))
                ),              
                Set(Q_Name("param", 1))                                      
            )
            ),
            List(Q_Skip()),
            Set(Q_Name("fun", 0))
        ), TypeInfo(Map(Q_Name("param", 0) -> KnownType.Int, Q_Name("param", 1) -> KnownType.Int), Map(Q_Name("fun", 0) -> (KnownType.Int, List(Q_Name("param", 0)))))
        )
    }

    it should "be able to shadow the function name" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, 
                    "fun", 
                    List(),
                    List(
                        Decl(
                            BaseType.Int, 
                            Ident("fun"), 
                            IntLiteral(0)
                        ),
                        Return(IntLiteral(0))
                    )
                )
            ),
            List(Skip.instance())
        )

        rename(prog) shouldBe (Q_Prog(
            List(
            Q_Func(
                BaseType.Int, 
                Q_Name("fun", 0), 
                List(), 
                List(
                Q_Decl(
                    Q_Name("fun", 1), 
                    Q_IntLiteral(0)
                ),
                Q_Return(Q_IntLiteral(0))
                ),              
                Set(Q_Name("fun", 1))                                      
            )
            ),
            List(Q_Skip()),
            Set(Q_Name("fun", 0))
        ), TypeInfo(Map(Q_Name("fun", 1) -> KnownType.Int), Map(Q_Name("fun", 0) -> (KnownType.Int, List())))
        )
    }

    it should "be able to shadow parameters with different types" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, 
                    "fun", 
                    List(Param(BaseType.String, "param")),
                    List(
                        Decl(
                            BaseType.Int, 
                            Ident("param"), 
                            IntLiteral(0)
                        ),
                        Return(IntLiteral(0))
                    )
                )
            ),
            List(Skip.instance())
        )

        rename(prog) shouldBe (Q_Prog(
            List(
            Q_Func(
                BaseType.Int, 
                Q_Name("fun", 0), 
                List(Q_Param(BaseType.String, Q_Name("param", 0))), 
                List(
                Q_Decl(
                    Q_Name("param", 1), 
                    Q_IntLiteral(0)
                ),
                Q_Return(Q_IntLiteral(0))
                ),              
                Set(Q_Name("param", 1))                                      
            )
            ),
            List(Q_Skip()),
            Set(Q_Name("fun", 0))
        ), TypeInfo(Map(Q_Name("param", 0) -> KnownType.String, Q_Name("param", 1) -> KnownType.Int), Map(Q_Name("fun", 0) -> (KnownType.Int, List(Q_Name("param", 0)))))
        )
    }
}
