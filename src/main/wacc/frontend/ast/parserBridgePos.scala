package wacc.ast

import parsley.generic
import parsley.Parsley
import parsley.ap.{ap1, ap2, ap3, ap4}
import parsley.position.pos


/** 
  * Generic bridge trait enabling the `<#`/`from` combinator on this type:
  * this is useful when the constructor is not applied immediately,
  * like when using `precedence`. It does not track any metadata.
  */
trait ParserSingletonBridgePos[+A] extends generic.ErrorBridge {
    /** 
      * The abstract hook method: what value is the singleton representing?
      * @see [[parsley.generic.ErrorBridge]]
      */
    protected def con(pos: (Int, Int)): A

    /** 
      * The syntax on this implementing type that performs the parser and returns `con`.
      * @param op the parser that should be parsed before returning `con`.
      * @note equivalent to `from`.
      */
    final def <#(op: Parsley[?]): Parsley[A] = this.from(op)

    /**
      * The combinator on this implementing type that performs the parser and returns `con`
      * @param op the parser that should be parsed before returning `con`.
      */
    def from(op: Parsley[?]): Parsley[A] = error(pos.map(this.con(_)) <* op)
}

/** $bridgefor 0 arguments. */
trait ParserBridgePos0[+B] extends ParserSingletonBridgePos[B] {
    /**
      * Abstract hook method for storing position information implicitly.
      * @param pos position information (line, char) used for locating errors.
      */
    def apply(pos: (Int, Int)): B

    /**
      * An apply method that wraps this node inside  [[Parsely]] object.
      * Allows us to invoke this constructor inside parser combinators.
      */
    def apply(): Parsley[B] = error(pos.map(con))

    /** @inheritdoc */
    override final def con(pos: (Int, Int)):B = this(pos)
}

/** $bridgefor 1 argument. */
trait ParserBridgePos1[-A, +B] extends ParserSingletonBridgePos[A => B] {
     /**
      * Abstract hook method for storing position information implicitly.
      * @param pos position information (line, char) used for locating errors.
      */
    def apply(a: A)(pos: (Int, Int)): B

    /**
      * An apply method that wraps this node inside  [[Parsely]] object.
      * Allows us to invoke this constructor inside parser combinators.
      */
    def apply(a: Parsley[A]): Parsley[B] = error(ap1(pos.map(con), a))

    /** @inheritdoc */
    override final def con(pos: (Int, Int)): A => B = this.apply(_)(pos)
}

/** $bridgefor 2 argument. */
trait ParserBridgePos2[-A, -B, +C] extends ParserSingletonBridgePos[(A, B) => C] {

    /**
      * Abstract hook method for storing position information implicitly.
      * @param pos position information (line, char) used for locating errors.
      */
    def apply(a: A, b: B)(pos: (Int, Int)): C

    /**
      * An apply method that wraps this node inside  [[Parsely]] object.
      * Allows us to invoke this constructor inside parser combinators.
      */
    def apply(a: Parsley[A], b: =>Parsley[B]): Parsley[C] = error(ap2(pos.map(con), a, b))

    /** @inheritdoc */
    override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)
}
/** $bridgefor 3 argument. */
trait ParserBridgePos3[-A, -B, -C, +D] extends ParserSingletonBridgePos[(A, B, C) => D] {
    /**
      * Abstract hook method for storing position information implicitly.
      * @param pos position information (line, char) used for locating errors.
      */
    def apply(a: A, b: B, c: C)(pos: (Int, Int)): D

    /**
      * An apply method that wraps this node inside  [[Parsely]] object.
      * Allows us to invoke this constructor inside parser combinators.
      */
    def apply(a: Parsley[A], b: =>Parsley[B], c: =>Parsley[C]): Parsley[D] = error(ap3(pos.map(con), a, b, c))

    /** @inheritdoc */
    override final def con(pos: (Int, Int)): (A, B, C) => D = this.apply(_, _, _)(pos)
}

/** $bridgefor 4 argument. */
trait ParserBridgePos4[-A, -B, -C, -D, +E] extends ParserSingletonBridgePos[(A, B, C, D) => E] {
    /**
      * Abstract hook method for storing position information implicitly.
      * @param pos position information (line, char) used for locating errors.
      */
    def apply(a: A, b: B, c: C, d:D)(pos: (Int, Int)): E
        
    /**
      * An apply method that wraps this node inside  [[Parsely]] object.
      * Allows us to invoke this constructor inside parser combinators.
      */
    def apply(
        a: Parsley[A], 
        b: => Parsley[B], 
        c: => Parsley[C], 
        d: => Parsley[D]
    ): Parsley[E] = error(ap4(pos.map(con), a, b, c, d))

    /** @inheritdoc */
    override final def con(pos: (Int, Int)): (A, B, C, D) => E = this.apply(_, _, _, _)(pos)
}
