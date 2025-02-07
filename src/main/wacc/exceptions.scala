package wacc
class ScopeException(messages: List[Err]) extends Exception()
class SyntaxFailureException(message: String) extends Exception(message)