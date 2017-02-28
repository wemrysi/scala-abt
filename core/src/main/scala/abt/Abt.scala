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

import matryoshka._
import scalaz._

/** Abstract Binding Tree
  * @tparam V Type of variables
  * @tparam S Sort (syntactic category)
  * @tparam O underlying AST (Operators)
  * @tparam T Abt concrete instance
  */
trait Abt[V, S, O[_], T] {
  import View._

  type Error        = AbtError[S, V]
  type MError[M[_]] = MonadError[M, Error]

  /** Construct an ABT from a view, validating it against the given valence. */
  // TODO: Better name, not a big fan of 'check' but 'into' isn't any better.
  def into[M[_]: MError](view: View[V, O, T], valence: Valence[S]): M[T]

  /** Pattern match on an ABT and its valence. */
  def infer(t: T): (View[V, O, T], Valence[S])

  /** Returns whether two ABTs are alpha equivalent. */
  def aequiv(x: T, y: T): Boolean = ???

  /** The free variables within the given ABT. */
  def freeVars[F[_]: PlusEmpty](abt: T): F[(V, S)] = ???

  /** Rename `from` to `to` in an ABT. */
  def rename[M[_]: MError](from: V, to: V)(implicit V: Equal[V], O: Functor[O]): T => M[T] = {
    val renamef: ElgotAlgebraM[(T, ?), M, View[V, O, ?], T] = {
      case (t, Abs(vs, a)) => ???
      case (t, Op(o))      => ???
      case (t, Var(v))     => ???
    }

    // TODO: Need to add elgotParamM to Matryoshka
    //_.elgotParaM(renamef)
    ???
  }

  /** Returns the sort of an ABT. */
  def sort(t: T): S =
    valence(t).sort

  /** Substitutes `t` for `x` in `body`, avoiding capture. */
  def subst(t: T, x: V): T => T = ???

  /** Returns the valence of an ABT. */
  def valence(t: T): Valence[S] =
    infer(t)._2

  /** Pattern match on an ABT. */
  def view(t: T): View[V, O, T] =
    infer(t)._1
}

object Abt extends AbtInstances {
  def apply[V, S, O[_], T](implicit A: Abt[V, S, O, T]): Abt[V, S, O, T] = A
}

sealed abstract class AbtInstances {
  implicit def abtRecursive[V, S, O[_], T](implicit A: Abt[V, S, O, T]): Recursive.Aux[T, View[V, O, ?]] =
    new Recursive[T] {
      type Base[A] = View[V, O, A]

      def project(t: T)(implicit BF: Functor[Base]): View[V, O, T] =
        A.view(t)
    }
}
