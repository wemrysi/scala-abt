/*
 * Copyright 2014–2017 SlamData Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package abt

import slamdata.Predef._

import scalaz._

/** Abstract Binding Tree
  * @tparam V Type of variables
  * @tparam S Sort (syntactic category)
  * @tparam O underlying AST (Operators)
  * @tparam T Abt concrete instance
  */
trait Abt[V, S, O[_], T] {
  /** Construct an ABT from a view, validating it against the given sort. */
  // TODO: Better name, not a big fan of 'check' but 'into' isn't any better.
  def into[F[_]](view: View[V, O, T], sort: S)(implicit ME: MonadError[F, AbtError[S, V]]): F[T]

  /** Pattern match on an ABT and its sort. */
  def infer(t: T): (View[V, O, T], S)

  /** Pattern match on an ABT. */
  def view(t: T): View[V, O, T] =
    infer(t)._1

  /** Returns whether two ABTs are alpha equivalent. */
  def aequiv(x: T, y: T): Boolean = ???

  /** The free variables within the given ABT. */
  def freeVars[F[_]: PlusEmpty](abt: T): F[(V, S)] = ???

  /** Rename `from` to `to` in an ABT. */
  def rename(from: V, to: V): T => T = ???

  /** Returns the sort of an ABT. */
  def sort(t: T): S =
    infer(t)._2

  /** Substitutes `t` for `x` in `body`, avoiding capture. */
  def subst(t: T, x: V): T => T = ???
}

object Abt {
  def apply[V, S, O[_], T](implicit A: Abt[V, S, O, T]): Abt[V, S, O, T] = A
}
