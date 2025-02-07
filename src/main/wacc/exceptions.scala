package wacc
class ScopeException(val messages: List[Err]) extends Exception()
class SyntaxFailureException(message: String) extends Exception(message)