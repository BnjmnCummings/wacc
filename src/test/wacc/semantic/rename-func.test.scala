package test.wacc.semantic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.ast.*
import wacc.q_ast.*
import wacc.renamer.*
import org.scalactic.Fail
import wacc.ScopeException


class rename_func_test extends AnyFlatSpec {

    /* control test */
    "rename (func)" should "be able to rename programs with functions" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, "fun", List(), // int fun()
                    List(Return(IntLiteral(0)))  // return 0
                )
            ),
            List(Skip)
        )

        rename(prog) shouldBe Q_Prog(
            List(
                Q_Func(
                    BaseType.Int, Q_Name("fun", "fun/0"), List(), // int fun/0()
                    List(Q_Return(Q_IntLiteral(0))),              // return 0  
                    Set()                                         // No locals
                )
            ),
            List(Q_Skip),
            Set(
                Q_Name("fun", "fun/0")
            )
        )
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
            List(Skip)
        )

        rename(prog) shouldBe Q_Prog(
            List(
                Q_Func(
                    BaseType.Int, 
                    Q_Name("fun", "fun/0"), 
                    List(Q_Param(BaseType.Int, Q_Name("aryaNarang", "aryaNarang/0"))), 
                    List(Q_Return(Q_IntLiteral(0))),              
                    Set(Q_Name("aryaNarang", "aryaNarang/0"))                                      
                )
            ),
            List(Q_Skip),
            Set(
                Q_Name("fun", "fun/0")
            )
        )
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
            List(Skip)
        )

        rename(prog) shouldBe Q_Prog(
            List(
                Q_Func(
                    BaseType.Int, 
                    Q_Name("fun", "fun/0"), 
                    List(Q_Param(BaseType.Int, Q_Name("aryaNarang", "aryaNarang/0"))), 
                    List(Q_Return(Q_IntLiteral(0))),              
                    Set(Q_Name("aryaNarang", "aryaNarang/0"))                                      
                ),

                Q_Func(
                    BaseType.Int, 
                    Q_Name("fun2", "fun2/0"), 
                    List(Q_Param(BaseType.Int, Q_Name("tejasMungale", "tejasMungale/0"))), 
                    List(Q_Return(Q_IntLiteral(0))),              
                    Set(Q_Name("tejasMungale", "tejasMungale/0"))                                      
                )
            ),
            List(Q_Skip),
            Set(
                Q_Name("fun2", "fun2/0"),
                Q_Name("fun", "fun/0"),
               
            )
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
            List(Skip)
        )

        rename(prog) shouldBe Q_Prog(
            List(
                Q_Func(
                    BaseType.Int, 
                    Q_Name("fun", "fun/0"), 
                    List(), 
                    List(
                        Q_Decl(
                            BaseType.Int, 
                            Q_Name("myVar", "myVar/0"), 
                            Q_IntLiteral(0)
                        ),
                        Q_Return(Q_IntLiteral(0))
                    ),              
                    Set(Q_Name("myVar", "myVar/0"))                                      
                )
            ),
            List(Q_Skip),
            Set(
                Q_Name("fun", "fun/0")
            )
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
            List(Skip)
        )

        rename(prog) shouldBe Q_Prog(
            List(
                Q_Func(
                    BaseType.Int, 
                    Q_Name("fun", "fun/0"), 
                    List(), 
                    List(
                        Q_Decl(
                            BaseType.Int, 
                            Q_Name("myVar", "myVar/0"), 
                            Q_IntLiteral(0)
                        ),
                        Q_Asgn(
                            Q_Ident(Q_Name("myVar", "myVar/0")), 
                            Q_IntLiteral(0)
                        ),
                        Q_Return(Q_IntLiteral(0))
                    ),              
                    Set(Q_Name("myVar", "myVar/0"))                                      
                )
            ),
            List(Q_Skip),
            Set(
                Q_Name("fun", "fun/0")
            )
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
            List(Skip)
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
            List(Skip)
        )

        a [ScopeException] should be thrownBy rename(prog)
    }

    it should "fail variables shadowed by parameters" in {
        val prog = Prog(
            List(
                Func(
                    BaseType.Int, 
                    "fun", 
                    List(
                        Param(BaseType.Int, "myVar")
                    ),
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
            List(Skip)
        )

        a [ScopeException] should be thrownBy rename(prog)
    }
}
