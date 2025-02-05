package wacc.ast

import wacc.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure, Result}
import wacc.lexer.fully

class prog_test extends AnyFlatSpec {
    "prog" should "be able to parse a program with no functions" in {
        parser.parse("begin skip end") shouldBe Success(
            Prog(
                Nil,
                List(Skip)
            )
        )
    }

    it should "be able to parse a program with one function" in {
        parser.parse("begin int fun() is return 0 end skip end") shouldBe Success(
            Prog(
                List(Func(BaseType.Int, "fun", List(), List(Return(IntLiteral(0))))),
                List(Skip)
            )
        )
    }

    it should "be able to parse a program with more than one function" in {
        parser.parse("begin int fun() is return 0 end int fun() is return 0 end skip end") shouldBe Success(
            Prog(
                List(
                    Func(BaseType.Int, "fun", List(), List(Return(IntLiteral(0)))),
                    Func(BaseType.Int, "fun", List(), List(Return(IntLiteral(0))))
                ),
                List(Skip)
            )
        )
    }

    it should "reject a program with no statements" in {
        parser.parse("begin int fun() is return 0 end end") shouldBe a [Failure[?]]
    }
    
    it should "reject a program with no begin end" in {
        parser.parse("skip") shouldBe a [Failure[?]]
    }

    it should "reject a program with no statements or functions" in {
        parser.parse("begin end") shouldBe a [Failure[?]]
    }
}


class func_test extends AnyFlatSpec {
    "func" should "be able to parse a function with no parameters" in {
        parser.func.parse("int fun() is return 0 end") shouldBe Success(
            Func(
                BaseType.Int,
                "fun",
                List(),
                List(Return(IntLiteral(0)))
            )
        )
    }

    it should "be able to parse a function with one parameter" in {
        parser.func.parse("int fun(int input) is return input end") shouldBe Success(
            Func(
                BaseType.Int,
                "fun",
                List(
                    Param(BaseType.Int, "input")
                ),
                List(Return(Ident("input")))
            )
        )
    }

    it should "fail missing keywords" in {
        parser.func.parse("int fun() is return 0 ") shouldBe a [Failure[?]]
        parser.func.parse("int fun() return 0 end") shouldBe a [Failure[?]]
        parser.func.parse("fun() is return 0 end")  shouldBe a [Failure[?]]
        parser.func.parse("int () is return 0 end") shouldBe a [Failure[?]]
    }

    it should "fail missing return keywords" in {
        parser.func.parse("int fun() is skip end") shouldBe a [Failure[?]]

        parser.func.parse{
            """
                int fun() is
                    if(true) then 
                        return 0 
                    else 
                        skip
                    fi
                end
            """.trim.replaceAll("\\s+", " ")
        } shouldBe a [Failure[?]]

        parser.func.parse{
            """
                int fun() is
                    if(true) then 
                        skip 
                    else 
                        return 0 
                    fi
                end
            """.trim.replaceAll("\\s+", " ")
        } shouldBe a [Failure[?]]

        parser.func.parse {
            """
                int fun() is
                    if(true) then 
                        return 0 
                    else 
                        return 0 
                    fi;
                    skip
                end
            """.trim.replaceAll("\\s+", " ")
        } shouldBe a [Failure[?]]

        parser.func.parse("fun() is return 0; skip end")  shouldBe a [Failure[?]]
        parser.func.parse("fun() is exit 0; skip end")  shouldBe a [Failure[?]]
    }

    it should "be able to parse a function with and alternative return" in {
        parser.func.parse("int fun() is exit 0 end") shouldBe Success(
            Func(
                BaseType.Int,
                "fun",
                List(),
                List(Exit(IntLiteral(0)))
            )
        )

        parser.func.parse {
            """
                int fun() is
                    if(true) then 
                        return 0 
                    else 
                        exit 0
                    fi
                end
            """.trim.replaceAll("\\s+", " ")
        } shouldBe Success(
            Func(
                BaseType.Int,
                "fun",
                List(),
                List(
                    If(
                        BoolLiteral(true), 
                        List(Return(IntLiteral(0))),
                        List(Exit(IntLiteral(0))),
                    )
                )
            )
        )
    }

    it should "be able to parse a function with a returning statement wrapped in a begin ... end codeblock" in {
        parser.func.parse("char five() is begin return \'5\' end end") shouldBe Success(
            Func(
                BaseType.Char,
                "five",
                List(),
                List(CodeBlock(List(Return(CharLiteral('5')))))
            )
        )
    }
}


class param_test extends AnyFlatSpec {
    "params" should "be able to parse a single parameter" in {
        parser.params.parse("int param1") shouldBe Success(
            List(
                Param(BaseType.Int, "param1")
            )
        )
    }

    it should "be able to parse many parametes" in {
        parser.params.parse("int param1, int param2") shouldBe Success(
            List(
                Param(BaseType.Int, "param1"),
                Param(BaseType.Int, "param2")
            )
        )

        parser.params.parse("int param1, string param2") shouldBe Success(
            List(
                Param(BaseType.Int, "param1"),
                Param(BaseType.String, "param2")
            )
        )

        parser.params.parse("int param1, string param2, bool param3 ") shouldBe Success(
            List(
                Param(BaseType.Int, "param1"),
                Param(BaseType.String, "param2"),
                Param(BaseType.Bool, "param3")
            )
        )
    }

    it should "fail invalid types" in {
        fully(parser.params).parse("fakeType param1")  shouldBe a [Failure[?]]
        fully(parser.params).parse("intint param1")    shouldBe a [Failure[?]]
        fully(parser.params).parse("stringint param1") shouldBe a [Failure[?]]
    }

    it should "fail missing commas" in {
        fully(parser.params).parse("int param1 int param2") shouldBe a [Failure[?]]
        fully(parser.params).parse("int param param2")      shouldBe a [Failure[?]]
        fully(parser.params).parse("param1")                shouldBe a [Failure[?]]
    }
}
