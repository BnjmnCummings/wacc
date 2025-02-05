package wacc.semantic

import wacc.syntax.* 
import collection.mutable

case class TypedProg(funcs: List[TypedFunc], body: List[TypedStmt])
case class TypedFunc(t: Type, id: Ident, args: List[TypedParam], body: List[TypedStmt])
case class TypedParam(t: Type, id: Ident)

def typeCheck(prog@(funcs: List[Func], body: List): Prog, tyInfo: TypeInfo): Either[List[Error], TypedProg] = { // Note this List is non-empty. Import won't work
    given ctx: TypeCheckerCtx[List[Error]] = TypeCheckerCtx(tyInfo, List.newBuilder)
    val typedProg = prog.map(check) // prog is not a list of stmt?? CHECK THIS!

    ctx.errors.match {
        case err :: errs => Left(err :: errs)
        case Nil         => Right(typedProg)
    }
}

class TypeCheckerCtx[C](tyInfo: TypeInfo, errs: mutable.Builder[Error, C]) {
    def errors: C = errs.result()

    // This will get the type of variables
    def typeOf(id: String): KnownType = tyInfo.varTys(id)

    def error(err: Error) = {
        errs += err
        None
    }
}

class TypeInfo(
    var varTys: Map[String, KnownType],
    var funcTys: Map[String, KnownType, List[KnownType]] // Check with Aidan to see how this would work
)