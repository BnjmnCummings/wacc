package wacc.semantic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.ast.*
import wacc.q_ast.*
import wacc.renamer.*
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
                    BaseType.Int, Name("fun", 0), List(), // int fun/0()
                    List(Q_Return(Q_IntLiteral(0))),              // return 0  
                    Set()                                         // No locals
                )
            ),
            List(Q_Skip()),
            Set(
                Name("fun", 0)
            )
        ), TypeInfo(Map(), Map(Name("fun", 0) -> (KnownType.Int, List()))))
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
                    Name("fun", 0), 
                    List(Q_Param(BaseType.Int, Name("aryaNarang", 0))), 
                    List(Q_Return(Q_IntLiteral(0))),              
                    Set()                                      
                )
            ),
            List(Q_Skip()),
            Set(
                Name("fun", 0)
            )
        ), TypeInfo(Map(Name("aryaNarang", 0) -> KnownType.Int), Map(Name("fun", 0) -> (KnownType.Int, List(Name("aryaNarang", 0))))))
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
                    Name("fun", 0), 
                    List(Q_Param(BaseType.Int, Name("aryaNarang", 0))), 
                    List(Q_Return(Q_IntLiteral(0))),              
                    Set()                                      
                ),

                Q_Func(
                    BaseType.Int, 
                    Name("fun2", 0), 
                    List(Q_Param(BaseType.Int, Name("tejasMungale", 0))), 
                    List(Q_Return(Q_IntLiteral(0))),              
                    Set()                                      
                )
            ),
            List(Q_Skip()),
            Set(
                Name("fun2", 0),
                Name("fun", 0),
               
            )
        ), TypeInfo(Map(Name("aryaNarang", 0) -> KnownType.Int, Name("tejasMungale", 0) -> KnownType.Int), Map(Name("fun", 0) -> (KnownType.Int, List(Name("aryaNarang", 0))), Name("fun2", 0) -> (KnownType.Int, List(Name("tejasMungale", 0)))))
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
                    Name("fun", 0), 
                    List(Q_Param(BaseType.Int, Name("aryaNarang", 0))), 
                    List(
                        Q_Decl(
                            Name("x", 0),
                            Q_Ident(Name("aryaNarang", 0)),
                        ),
                        Q_Return(Q_IntLiteral(0))
                    ),              
                    Set(Name("x", 0))                                      
                )
            ),
            List(Q_Skip()),
            Set(
                Name("fun", 0)
            )
        ), TypeInfo(Map(Name("aryaNarang", 0) -> KnownType.Int, Name("x", 0) -> KnownType.Int), Map(Name("fun", 0) -> (KnownType.Int, List(Name("aryaNarang", 0))))))
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
                    Name("fun", 0), 
                    List(Q_Param(BaseType.Int, Name("aryaNarang", 0))), 
                    List(
                        Q_Asgn(
                            Q_Ident(Name("aryaNarang", 0)),
                            Q_IntLiteral(0)
                        ),
                        Q_Return(Q_IntLiteral(0))
                    ),              
                    Set()                                      
                )
            ),
            List(Q_Skip()),
            Set(
                Name("fun", 0)
            )
        ), TypeInfo(Map(Name("aryaNarang", 0) -> KnownType.Int), Map(Name("fun", 0) -> (KnownType.Int, List(Name("aryaNarang", 0)))))
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
                    Name("fun", 0), 
                    List(), 
                    List(
                        Q_Decl(
                            Name("myVar", 0), 
                            Q_IntLiteral(0)
                        ),
                        Q_Return(Q_IntLiteral(0))
                    ),              
                    Set(Name("myVar", 0))                                      
                )
            ),
            List(Q_Skip()),
            Set(
                Name("fun", 0)
            )
        ), TypeInfo(Map(Name("myVar", 0) -> KnownType.Int), Map(Name("fun", 0) -> (KnownType.Int, List())))
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
                    Name("fun", 0), 
                    List(), 
                    List(
                        Q_Decl(
                            Name("myVar", 0), 
                            Q_IntLiteral(0)
                        ),
                        Q_Asgn(
                            Q_Ident(Name("myVar", 0)), 
                            Q_IntLiteral(0)
                        ),
                        Q_Return(Q_IntLiteral(0))
                    ),              
                    Set(Name("myVar", 0))                                      
                )
            ),
            List(Q_Skip()),
            Set(
                Name("fun", 0)
            )
        ), TypeInfo(Map(Name("myVar", 0) -> KnownType.Int), Map(Name("fun", 0) -> (KnownType.Int, List())))
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
                    Name("fun", 0), 
                    List(Q_Param(BaseType.Int, Name("fun", 1))), 
                    List(Q_Return(Q_IntLiteral(0))),              
                    Set()                                      
                )
            ),
            List(Q_Skip()),
            Set(
                Name("fun", 0)
            )
        ), TypeInfo(Map(Name("fun", 1) -> KnownType.Int), Map(Name("fun", 0) -> (KnownType.Int, List(Name("fun", 1)))))
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
                Name("fun", 0), 
                List(Q_Param(BaseType.Int, Name("param", 0))), 
                List(
                Q_Decl(
                    Name("param", 1), 
                    Q_IntLiteral(0)
                ),
                Q_Return(Q_IntLiteral(0))
                ),              
                Set(Name("param", 1))                                      
            )
            ),
            List(Q_Skip()),
            Set(Name("fun", 0))
        ), TypeInfo(Map(Name("param", 0) -> KnownType.Int, Name("param", 1) -> KnownType.Int), Map(Name("fun", 0) -> (KnownType.Int, List(Name("param", 0)))))
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
                Name("fun", 0), 
                List(), 
                List(
                Q_Decl(
                    Name("fun", 1), 
                    Q_IntLiteral(0)
                ),
                Q_Return(Q_IntLiteral(0))
                ),              
                Set(Name("fun", 1))                                      
            )
            ),
            List(Q_Skip()),
            Set(Name("fun", 0))
        ), TypeInfo(Map(Name("fun", 1) -> KnownType.Int), Map(Name("fun", 0) -> (KnownType.Int, List())))
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
                Name("fun", 0), 
                List(Q_Param(BaseType.String, Name("param", 0))), 
                List(
                Q_Decl(
                    Name("param", 1), 
                    Q_IntLiteral(0)
                ),
                Q_Return(Q_IntLiteral(0))
                ),              
                Set(Name("param", 1))                                      
            )
            ),
            List(Q_Skip()),
            Set(Name("fun", 0))
        ), TypeInfo(Map(Name("param", 0) -> KnownType.String, Name("param", 1) -> KnownType.Int), Map(Name("fun", 0) -> (KnownType.Int, List(Name("param", 0)))))
        )
    }
}
