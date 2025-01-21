enum Expr {
  // Binary operators
  case Mul(x: Expr, y: Expr)
  case Div(x: Expr, y: Expr)
  case Mod(x: Expr, y: Expr)
  case Add(x: Expr, y: Expr)
  case Sub(x: Expr, y: Expr)
  case GreaterThan(x: Expr, y: Expr)
  case GreaterThanEq(x: Expr, y: Expr)
  case LessThan(x: Expr, y: Expr)
  case LessThanEq(x: Expr, y: Expr)
  case Eq(x: Expr, y: Expr)
  case NotEq(x: Expr, y: Expr)
  case And(x: Expr, y: Expr)
  case Or(x: Expr, y: Expr)

  // Unary operators
  case Not(x: Expr)
  case Neg(x: Expr)
  case Len(x: Expr)
  case Ord(x: Expr)
  case Chr(x: Expr)

  // Atoms

}