package wacc.syntax

import parsley.Parsley
import parsley.position.pos


// From the parsley wiki

trait ParserBridgePos1[-A, +B] {
    def apply(x: A)(pos: (Int, Int)): B
    def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(this.apply(_))
    def from[T](op: Parsley[T]): Parsley[A => B] =
        pos.map[A => B](p => this.apply(_)(p)) <* op
    final def <#[T](op: Parsley[T]): Parsley[A => B] = this from op
}

trait ParserBridgePos2[-A, -B, +C] {
    def apply(x: A, y: B)(pos: (Int, Int)): C
    def apply(x: Parsley[A], y: Parsley[B]): Parsley[C] =
        pos <**> (x, y).zipped(this.apply(_, _) _)
    def from[T](op: Parsley[T]): Parsley[(A, B) => C] =
        pos.map[(A, B) => C](p => this.apply(_, _)(p)) <* op
    final def <#[T](op: Parsley[T]): Parsley[(A, B) => C] = this from op
}

trait ParserBridgePos3[-A, -B, -C, +D] {
    def apply(x: A, y: B, z: C)(pos: (Int, Int)): D
    def from[T](op: Parsley[T]): Parsley[(A, B, C) => D] =
        pos.map[(A, B, C) => D](p => this.apply(_, _, _)(p)) <* op
    final def <#[T](op: Parsley[T]): Parsley[(A, B, C) => D] = this from op
}

trait ParserBridgePos4[-A, -B, -C, -D, +E] {
    def apply(x: A, y: B, z: C, w: D)(pos: (Int, Int)): E
    def from[T](op: Parsley[T]): Parsley[(A, B, C, D) => E] =
        pos.map[(A, B, C, D) => E](p => this.apply(_, _, _, _)(p)) <* op
    final def <#[T](op: Parsley[T]): Parsley[(A, B, C, D) => E] = this from op
}