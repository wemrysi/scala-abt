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
import View._

import monocle.Prism
import scalaz._, Scalaz._

/** Pattern functor for an ABT.
  * @tparam O Operator, the underlying AST
  * @tparam V Type of Variables in the ABT
  */
sealed trait View[V, O[_], A]

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object View extends ViewInstances {
  /** Variable binding. */
  final case class Abs[V, O[_], A](vs: Vector[V], a: A) extends View[V, O, A]
  /** A standard term in the underlying AST */
  final case class Op[V, O[_], A](op: O[A])             extends View[V, O, A]
  /** Variable reference. */
  final case class Var[V, O[_], A](v: V)                extends View[V, O, A]

  def vAbs[V, O[_], A]: Prism[View[V, O, A], (Vector[V], A)] =
    Prism.partial[View[V, O, A], (Vector[V], A)] {
      case Abs(vs, a) => (vs, a)
    } { case (vs, a) => Abs(vs, a) }

  def vOp[V, O[_], A]: Prism[View[V, O, A], O[A]] =
    Prism.partial[View[V, O, A], O[A]] {
      case Op(o) => o
    } (Op(_))

  def vVar[V, O[_], A]: Prism[View[V, O, A], V] =
    Prism.partial[View[V, O, A], V] {
      case Var(v) => v
    } (Var(_))
}

sealed abstract class ViewInstances extends ViewInstances2 {
  implicit def viewOrder[V: Order, O[_], A: Order](implicit O: Order[O[A]]): Order[View[V, O, A]] =
    Order.orderBy {
      case Abs(vs, a) => (some((vs, a)),    none,    none)
      case Op(o)      => (         none, some(o),    none)
      case Var(v)     => (         none,    none, some(v))
    }
}

sealed abstract class ViewInstances2 extends ViewInstances1 {
  implicit def viewEqual[V: Equal, O[_], A: Equal](implicit O: Equal[O[A]]): Equal[View[V, O, A]] =
    Equal.equalBy {
      case Abs(vs, a) => (some((vs, a)),    none,    none)
      case Op(o)      => (         none, some(o),    none)
      case Var(v)     => (         none,    none, some(v))
    }
}

sealed abstract class ViewInstances1 extends ViewInstances0 {
  implicit def viewTraverse[V, O[_]: Traverse]: Traverse[View[V, O, ?]] =
    new Traverse[View[V, O, ?]] {
      def traverseImpl[G[_]: Applicative, A, B](va: View[V, O, A])(f: A => G[B]): G[View[V, O, B]] =
        va match {
          case Abs(vs, a) => f(a) map (vAbs(vs, _))
          case Op(o)      => (o traverse f) map (vOp(_))
          case Var(v)     => vVar(v).point[G]
        }
    }
}

sealed abstract class ViewInstances0 {
  implicit def viewFoldable[V, O[_]: Foldable]: Foldable[View[V, O, ?]] =
    new Foldable.FromFoldMap[View[V, O, ?]] {
      def foldMap[A, B: Monoid](va: View[V, O, A])(f: A => B): B = va match {
        case Abs(_, a) => f(a)
        case Op(o)     => o foldMap f
        case Var(_)    => mzero[B]
      }
    }

  implicit def viewFunctor[V, O[_]: Functor]: Functor[View[V, O, ?]] =
    new Functor[View[V, O, ?]] {
      def map[A, B](va: View[V, O, A])(f: A => B): View[V, O, B] = va match {
        case Abs(vs, a) => vAbs(vs, f(a))
        case Op(o)      => vOp(o map f)
        case Var(v)     => vVar(v)
      }
    }
}
