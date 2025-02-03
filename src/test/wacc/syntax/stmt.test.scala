// package wacc.syntax

// import wacc.parser

// import org.scalatest.flatspec.AnyFlatSpec
// import org.scalatest.matchers.should.Matchers.*
// import parsley.{Success, Failure, Result}
// import wacc.lexer.fully

// class stmt_test extends AnyFlatSpec {
//     "skip" should "parse a skip statement" in {
//         parser.skip.parse("skip") shouldBe Success(Skip)
//     }

//     "decl" should "parse a valid declaration" in {
//         parser.decl.parse("int var = 6") shouldBe Success(
//             Decl(
//                 BaseType.Int,
//                 "var",
//                 IntLiteral(6)
//             )
//         )        
//     }

//     it should "fail invalid syntax" in {
//         fully(parser.decl).parse("int var 6") shouldBe a [Failure[?]]
//         fully(parser.decl).parse("var = 6") shouldBe a [Failure[?]]
//         fully(parser.decl).parse("int = 6") shouldBe a [Failure[?]]
//         fully(parser.decl).parse("int var == 6") shouldBe a [Failure[?]]
//     }

//     "asgn" should "parse a valid assignment" in {
//         parser.asgn.parse("var = 6") shouldBe Success(
//             Asgn(
//                 Ident("var"),
//                 IntLiteral(6)
//             )
//         ) 
//     }

//     it should "parse a valid assignment into an array" in {
//         parser.asgn.parse("arr[0] = 6") shouldBe Success(
//             Asgn(
//                 ArrayElem("arr", List(IntLiteral(0))),
//                 IntLiteral(6)
//             )
//         ) 

//         parser.asgn.parse("arr[0][0] = 6") shouldBe Success(
//             Asgn(
//                 ArrayElem("arr", List(IntLiteral(0), IntLiteral(0))),
//                 IntLiteral(6)
//             )
//         )
//     }

//     it should "parse a valid assignment into a pair" in {
//         parser.asgn.parse("fst pear = 6") shouldBe Success(
//             Asgn(
//                 PairElem(PairIndex.First, Ident("pear")),
//                 IntLiteral(6)
//             )
//         )

//         parser.asgn.parse("snd pear = 6") shouldBe Success(
//             Asgn(
//                 PairElem(PairIndex.Second, Ident("pear")),
//                 IntLiteral(6)
//             )
//         ) 
//     }

//     it should "fail invalid syntax" in {
//         fully(parser.decl).parse("int = 6") shouldBe a [Failure[?]]
//         fully(parser.decl).parse("202 = 6") shouldBe a [Failure[?]]
//         fully(parser.decl).parse("6 = 6") shouldBe a [Failure[?]]
//         fully(parser.decl).parse("[1,2,3] = 6") shouldBe a [Failure[?]]
//         fully(parser.decl).parse("var = 6 = 2") shouldBe a [Failure[?]]
//     }

//     "Read" should "read into a left-hand value" in {
//         parser.read.parse("read v") shouldBe Success(
//             Read(
//                 Ident("v") 
//             )
//         )
//     }

//     it should "read from an array lvalue" in {
//         parser.read.parse("read arr[0]") shouldBe Success(
//             Read(
//                 ArrayElem("arr", List(IntLiteral(0))) 
//             )
//         )
//     }

//     it should "read from a pair element lvalue" in {
//         parser.read.parse("read fst pear") shouldBe Success(
//             Read(
//                 PairElem(PairIndex.First, Ident("pear")) 
//             )
//         )
//     }


//     "Free" should "parse a free for an identity expression" in {
//         parser.free.parse("free v") shouldBe Success(
//             Free(
//                 Ident("v") 
//             )
//         )
//     }

//     it should "free an array expression" in {
//         parser.free.parse("free arr[0]") shouldBe Success(
//             Free(
//                 ArrayElem("arr", List(IntLiteral(0))) 
//             )
//         )
//     }

//     // it should "free a pair element expression" in {
//     //     parser.free.parse("free fst pear") shouldBe Success(
//     //         Free(
//     //             PairElem(PairIndex.First, Ident("pear")) 
//     //         )
//     //     )
//     // }

//     "Return" should "parse a return for an identity expression" in {
//         parser._return.parse("return v") shouldBe Success(
//             Return(
//                 Ident("v") 
//             )
//         )
//     }

//     it should "parse a return for a literal expression" in {
//         parser._return.parse("return 6") shouldBe Success(
//             Return(
//                 IntLiteral(6)
//             )
//         )
//     }

//     it should "parse a return for an array expression" in {
//         parser._return.parse("return arr[0]") shouldBe Success(
//             Return(
//                 ArrayElem("arr", List(IntLiteral(0))) 
//             )
//         )
//     }

//     // it should "parse a return for a pair element expression" in {
//     //     parser._return.parse("return fst pear") shouldBe Success(
//     //         Return(
//     //             PairElem(PairIndex.First, Ident("pear")) 
//     //         )
//     //     )
//     // }

//     "Exit" should "parse an exti for an identity expression" in {
//         parser.exit.parse("exit v") shouldBe Success(
//             Exit(
//                 Ident("v") 
//             )
//         )
//     }

//     it should "parse an exit for a literal expression" in {
//         parser.exit.parse("exit 6") shouldBe Success(
//             Exit(
//                 IntLiteral(6)
//             )
//         )
//     }

//     it should "parse an exit for an array expression" in {
//         parser.exit.parse("exit arr[0]") shouldBe Success(
//             Exit(
//                 ArrayElem("arr", List(IntLiteral(0))) 
//             )
//         )
//     }

//     // it should "parse an exit for a pair element expression" in {
//     //     parser.exit.parse("exit fst pear") shouldBe Success(
//     //         Exit(
//     //             PairElem(PairIndex.First, Ident("pear")) 
//     //         )
//     //     )
//     // }

//     "Print" should "parse a print for an identity expression" in {
//         parser.print.parse("print v") shouldBe Success(
//             Print(
//                 Ident("v") 
//             )
//         )
//     }

//     it should "parse a print for a literal expression" in {
//         parser.print.parse("print \"Hello World!\"") shouldBe Success(
//             Print(
//                 StringLiteral("Hello World!")
//             )
//         )
//     }

//     it should "parse a print for an array expression" in {
//         parser.print.parse("print arr[0]") shouldBe Success(
//             Print(
//                 ArrayElem("arr", List(IntLiteral(0))) 
//             )
//         )
//     }

//     // it should "parse a print for a pair element expression" in {
//     //     parser.print.parse("print fst pear") shouldBe Success(
//     //         Print(
//     //             PairElem(PairIndex.First, Ident("pear")) 
//     //         )
//     //     )
//     // }

//     "Println" should "parse a println for an identity expression" in {
//         parser.println.parse("println v") shouldBe Success(
//             Println(
//                 Ident("v") 
//             )
//         )
//     }

//     it should "parse a println for a literal expression" in {
//         parser.println.parse("println \"Hello World!\"") shouldBe Success(
//             Println(
//                 StringLiteral("Hello World!")
//             )
//         )
//     }

//     it should "parse a println for an array expression" in {
//         parser.println.parse("println arr[0]") shouldBe Success(
//             Println(
//                 ArrayElem("arr", List(IntLiteral(0))) 
//             )
//         )
//     }

//     // it should "parse a println for a pair element expression" in {
//     //     parser.println.parse("println fst pear") shouldBe Success(
//     //         Println(
//     //             PairElem(PairIndex.First, Ident("pear")) 
//     //         )
//     //     )
//     // }

//     "_if" should "parse valid if blocks" in {
//         parser._if.parse {
//             """
//                 if (true) then 
//                     skip 
//                 else 
//                     skip
//                 fi
//             """.trim.replaceAll("\\s+", " ")
//         } shouldBe Success(
//             If(
//                 BoolLiteral(true), 
//                 List(Skip),
//                 List(Skip)
//             )
//         )
//     }

//     it should "parse nested if blocks" in {
//         parser._if.parse {
//             """
//                 if (true) then 
//                     if( false) then 
//                         skip 
//                     else 
//                         skip
//                     fi
//                 else 
//                     skip
//                 fi
//             """.trim.replaceAll("\\s+", " ")
//         } shouldBe Success(
//             If(
//                 BoolLiteral(true), 
//                 List(
//                     If(
//                         BoolLiteral(false), 
//                         List(Skip),
//                         List(Skip)
//                     )
//                 ),
//                 List(Skip),
//             )
//         )

//         parser._if.parse {
//             """
//                 if (true) then 
//                     skip
//                 else 
//                     if (false) then 
//                         skip 
//                     else 
//                         skip
//                     fi
//                 fi
//             """.trim.replaceAll("\\s+", " ")
//         } shouldBe Success(
//             If(
//                 BoolLiteral(true), 
//                 List(Skip),
//                 List(
//                     If(
//                         BoolLiteral(false), 
//                         List(Skip),
//                         List(Skip),
//                     )
//                 )
//             )
//         )
//     }

//     it should "fail invalid syntax" in {
//         fully(parser._if).parse("if (true) then skip else skip") shouldBe a [Failure[?]]
//         fully(parser._if).parse("if (true) skip else skip fi") shouldBe a [Failure[?]]
//         fully(parser._if).parse("if (true) then skip fi") shouldBe a [Failure[?]]
//         fully(parser._if).parse("if (true) then skip else skip fi; skip") shouldBe a [Failure[?]]
//     }

//     "_while" should "parse valid while blocks" in {
//         parser._while.parse {
//             """
//                 while (true) do 
//                     skip 
//                 done
//             """.trim.replaceAll("\\s+", " ")
//         } shouldBe Success(
//             While(
//                 BoolLiteral(true), 
//                 List(Skip)
//             )
//         )
//     }

//     it should "parse nested while blocks" in {
//         parser._while.parse {
//             """
//                 while (true) do 
//                     while (true) do 
//                         skip 
//                     done 
//                 done
//             """.trim.replaceAll("\\s+", " ")
//         } shouldBe Success(
//             While(
//                 BoolLiteral(true), 
//                 List(
//                     While(
//                         BoolLiteral(true), 
//                         List(Skip)
//                     )
//                 )
//             )
//         )

//         parser._while.parse {
//             """
//                 while (true) do 
//                     while (true) do 
//                         skip 
//                     done;
//                     skip
//                 done
//             """.trim.replaceAll("\\s+", " ")
//         } shouldBe Success(
//             While(
//                 BoolLiteral(true), 
//                 List(
//                     While(
//                         BoolLiteral(true), 
//                         List(Skip)
//                     ),
//                     Skip
//                 )
//             )
//         )

//         parser._while.parse {
//             """
//                 while (true) do 
//                     while (true) do 
//                         skip 
//                     done;
//                     begin
//                         skip
//                     end;
//                     if (true) then 
//                         skip 
//                     else 
//                         skip
//                     fi
//                 done
//             """.trim.replaceAll("\\s+", " ")
//         } shouldBe Success(
//             While(
//                 BoolLiteral(true), 
//                 List(
//                     While(
//                         BoolLiteral(true), 
//                         List(Skip)
//                     ),
//                     CodeBlock(
//                         List(Skip)
//                     ),
//                     If(
//                         BoolLiteral(true), 
//                         List(Skip),
//                         List(Skip)
//                     )
//                 )
//             )
//         )
//     }

//     it should "fail invalid syntax" in {
//         fully(parser._while).parse("while (true) do skip done skip") shouldBe a [Failure[?]]
//         fully(parser._while).parse("while (true) skip done") shouldBe a [Failure[?]]
//         fully(parser._while).parse("while (true) do done") shouldBe a [Failure[?]]
//         fully(parser._while).parse("while (true) do skip") shouldBe a [Failure[?]]
//         fully(parser._while).parse("while do skip done") shouldBe a [Failure[?]]
//     }

//     "codeblock" should "parse a begin / end statement" in {
//         parser.codeblock.parse("begin skip end") shouldBe Success(
//             CodeBlock(
//                 List(
//                     Skip
//                 )
//             )
//         )
        
//         parser.codeblock.parse{
//             """
//                 begin
//                     println("Hello World!")
//                 end
//             """.trim.replaceAll("\\s+", " ")
//         } shouldBe Success(
//             CodeBlock(
//                 List(
//                     Println(
//                         StringLiteral("Hello World!")
//                     )
//                 )
//             )   
//         )
//     }

//     it should "fail missing begin or end statement" in {
//         fully(parser.codeblock).parse("skip end") shouldBe a [Failure[?]]
//         fully(parser.codeblock).parse("begin skip") shouldBe a [Failure[?]]
//     }

//     "stmts" should "parse a list of statements" in {
//         fully(parser.stmts).parse("skip") shouldBe Success(
//             List(Skip)
//         )
//         fully(parser.stmts).parse("skip; skip; skip; skip") shouldBe Success(
//             List(Skip, Skip, Skip, Skip)
//         )
//     }

//     it should "fail a list with missing semi colons" in {
//         fully(parser.codeblock).parse("skip skip skip skip") shouldBe a [Failure[?]]
//         fully(parser.codeblock).parse("skip; skip skip; skip") shouldBe a [Failure[?]]
//     }
// }  
