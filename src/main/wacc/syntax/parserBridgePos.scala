package wacc.syntax

import parsley.generic
import parsley.Parsley
import parsley.ap.{ap1, ap2, ap3, ap4}
import parsley.position.pos


trait ParserSingletonBridgePos[+A] extends generic.ErrorBridge {
    protected def con(pos: (Int, Int)): A
    def from(op: Parsley[?]): Parsley[A] = error(pos.map(this.con(_)) <* op)
    final def <#(op: Parsley[?]): Parsley[A] = this.from(op)
}

trait ParserBridgePos1[-A, +B] extends ParserSingletonBridgePos[A => B] {
    def apply(a: A)(pos: (Int, Int)): B
    def apply(a: Parsley[A]): Parsley[B] = error(ap1(pos.map(con), a))

    override final def con(pos: (Int, Int)): A => B = this.apply(_)(pos)
}

trait ParserBridgePos2[-A, -B, +C] extends ParserSingletonBridgePos[(A, B) => C] {
    //def apply(a: A, b: B): C = apply(a, b)((0,0))
    def apply(a: A, b: B)(pos: (Int, Int)): C
    def apply(a: Parsley[A], b: =>Parsley[B]): Parsley[C] = error(ap2(pos.map(con), a, b))

    override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)
}

trait ParserBridgePos3[-A, -B, -C, +D] extends ParserSingletonBridgePos[(A, B, C) => D] {
    // def apply(a: A, b: B, c: C): D = apply(a, b, c)((0,0))
    def apply(a: A, b: B, c: C)(pos: (Int, Int)): D
    def apply(a: Parsley[A], b: =>Parsley[B], c: =>Parsley[C]): Parsley[D] = error(ap3(pos.map(con), a, b, c))

    override final def con(pos: (Int, Int)): (A, B, C) => D = this.apply(_, _, _)(pos)
}

trait ParserBridgePos4[-A, -B, -C, -D, +E] extends ParserSingletonBridgePos[(A, B, C, D) => E] {
    // def apply(a: A, b: B, c: C, d:D): E = apply(a, b, c, d)((0,0))
    def apply(a: A, b: B, c: C, d:D)(pos: (Int, Int)): E

    def apply(
        a: Parsley[A], 
        b: => Parsley[B], 
        c: => Parsley[C], 
        d: => Parsley[D]
    ): Parsley[E] = error(ap4(pos.map(con), a, b, c, d))

    override final def con(pos: (Int, Int)): (A, B, C, D) => E = this.apply(_, _, _, _)(pos)
}
