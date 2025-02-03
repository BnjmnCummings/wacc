package wacc.syntax

import parsley.generic
import parsley.Parsley
import parsley.ap.{ap1, ap2, ap3, ap4}
import parsley.position.pos


// From the parsley wiki

trait ParserSingletonBridgePos[+A] extends generic.ErrorBridge {
    protected def con(pos: (Int, Int)): A
    def from(op: Parsley[?]): Parsley[A] = error(pos.map(this.con(_)) <* op)
    final def <#(op: Parsley[?]): Parsley[A] = this.from(op)
}

trait ParserBridgePos1[-A, +B] extends ParserSingletonBridgePos[A => B] {
    // we also want an apply function that takes in a partial function and returns the object before that
    // if we're given Ident("x") this now has type (Int, Int) => Ident
    // we want to be able to pass this in to a function that takes Ident
    // so we need to be able to convert it to Ident 
    // for testing
    // def apply(a:(Int, Int) => A): B = this.apply(a(0,0))(0,0)
    //def apply(x: A): B //= apply(x)((0,0))
    def apply(a: A)(pos: (Int, Int)): B
    // then the type of apply(a) is (Int, Int) => B
    def apply(a: Parsley[A]): Parsley[B] = error(ap1(pos.map(con), a))

    override final def con(pos: (Int, Int)): A => B = this.apply(_)(pos)
}

trait ParserBridgePos2[-A, -B, +C] extends ParserSingletonBridgePos[(A, B) => C] {
    //def apply(x: A, y: B): C //= apply(x, y)((0,0))
    def apply(a: A, b: B)(pos: (Int, Int)): C
    def apply(a: Parsley[A], b: =>Parsley[B]): Parsley[C] = error(ap2(pos.map(con), a, b))

    override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)
}

trait ParserBridgePos3[-A, -B, -C, +D] extends ParserSingletonBridgePos[(A, B, C) => D] {
    //def apply(a: A, b: B, c: C): D //= apply(a, b, c)((0,0))
    def apply(a: A, b: B, c: C)(pos: (Int, Int)): D
    def apply(a: Parsley[A], b: =>Parsley[B], c: =>Parsley[C]): Parsley[D] = error(ap3(pos.map(con), a, b, c))

    override final def con(pos: (Int, Int)): (A, B, C) => D = this.apply(_, _, _)(pos)
}

trait ParserBridgePos4[-A, -B, -C, -D, +E] extends ParserSingletonBridgePos[(A, B, C, D) => E] {
    //def apply(a: A, b: B, c: C, d:D): E = apply(a, b, c, d)((0,0))
    def apply(a: A, b: B, c: C, d:D)(pos: (Int, Int)): E

    def apply(
        a: Parsley[A], 
        b: => Parsley[B], 
        c: => Parsley[C], 
        d: => Parsley[D]
    ): Parsley[E] = error(ap4(pos.map(con), a, b, c, d))

    override final def con(pos: (Int, Int)): (A, B, C, D) => E = this.apply(_, _, _, _)(pos)
}

// trait ParserBridgePos1[-A, +B] {
//     def apply(x: A)(pos: (Int, Int)): B
//     private def con(pos: (Int, Int)): A => B = this.apply(_)(pos)

//     def from(op: Parsley[_]): Parsley[A => B] = pos.map(con) <* op
//     final def <#(op: Parsley[_]): Parsley[A => B] = this from op

// }
// trait ParserBridgePos2[-A, -B, +C] {
//     def apply(x: A, y: B)(pos: (Int, Int)): C
//     def apply(x: Parsley[A], y: Parsley[B]): Parsley[C] =
//         pos <**> (x, y).zipped(this.apply(_, _) _)
//     def from[T](op: Parsley[T]): Parsley[(A, B) => C] =
//         pos.map[(A, B) => C](p => this.apply(_, _)(p)) <* op
//     final def <#[T](op: Parsley[T]): Parsley[(A, B) => C] = this from op
// }

// trait ParserBridgePos3[-A, -B, -C, +D] {
//     def apply(x: A, y: B, z: C)(pos: (Int, Int)): D
//     def from[T](op: Parsley[T]): Parsley[(A, B, C) => D] =
//         pos.map[(A, B, C) => D](p => this.apply(_, _, _)(p)) <* op
//     final def <#[T](op: Parsley[T]): Parsley[(A, B, C) => D] = this from op
// }

// trait ParserBridgePos4[-A, -B, -C, -D, +E] {
//     def apply(x: A, y: B, z: C, w: D)(pos: (Int, Int)): E
//     def from[T](op: Parsley[T]): Parsley[(A, B, C, D) => E] =
//         pos.map[(A, B, C, D) => E](p => this.apply(_, _, _, _)(p)) <* op
//     final def <#[T](op: Parsley[T]): Parsley[(A, B, C, D) => E] = this from op
// }