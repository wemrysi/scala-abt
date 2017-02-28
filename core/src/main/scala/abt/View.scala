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

/**
 *
 * @tparam V Type of Variables in the ADT (kind of like identifier)
 * @tparam O The underlying AST
 */
sealed trait View[V, O, A]

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object View extends ViewInstances {
  /** A node that’s a variable reference */
  final case class Var[V, O, A](v: V)                 extends View[V, O, A]
  /** Abstraction is a node that binds variables */
  final case class Abs[V, O, A](vs: Vector[V], a: A)  extends View[V, O, A]
  /** A standard term in the underlying AST */
  final case class App[V, O, A](op: O, as: Vector[A]) extends View[V, O, A]

  def vVar[V, O, A]: Prism[View[V, O, A], V] =
    Prism.partial[View[V, O, A], V] {
      case Var(v) => v
    } (Var(_))

  def vAbs[V, O, A]: Prism[View[V, O, A], (Vector[V], A)] =
    Prism.partial[View[V, O, A], (Vector[V], A)] {
      case Abs(vs, a) => (vs, a)
    } { case (vs, a) => Abs(vs, a) }

  def vApp[V, O, A]: Prism[View[V, O, A], (O, Vector[A])] =
    Prism.partial[View[V, O, A], (O, Vector[A])] {
      case App(o, as) => (o, as)
    } { case (o, as) => App(o, as) }
}

sealed abstract class ViewInstances extends ViewInstances2 {
  implicit def viewOrder[V: Order, O: Order, A: Order]: Order[View[V, O, A]] =
    Order orderBy {
      case Var(v)     => (some(v), none         ,          none)
      case Abs(vs, a) => (none   , some((vs, a)),          none)
      case App(o, as) => (none   , none         , some((o, as)))
    }
}

sealed abstract class ViewInstances2 extends ViewInstances1 {
  implicit def viewEqual[V: Equal, O: Equal, A: Equal]: Equal[View[V, O, A]] =
    Equal equalBy {
      case Var(v)     => (some(v), none         ,          none)
      case Abs(vs, a) => (none   , some((vs, a)),          none)
      case App(o, as) => (none   , none         , some((o, as)))
    }
}

sealed abstract class ViewInstances1 extends ViewInstances0 {
  implicit def viewTraverse[V, O]: Traverse[View[V, O, ?]] =
    new Traverse[View[V, O, ?]] {
      def traverseImpl[G[_]: Applicative, A, B](va: View[V, O, A])(f: A => G[B]): G[View[V, O, B]] =
        va match {
          case Var(v)      => vVar(v).point[G]
          case Abs(vs, a)  => f(a) map (vAbs(vs, _))
          case App(op, as) => (as traverse f) map (vApp(op, _))
        }
    }
}

sealed abstract class ViewInstances0 {
  implicit def viewFoldable[V, O]: Foldable[View[V, O, ?]] =
    new Foldable.FromFoldMap[View[V, O, ?]] {
      def foldMap[A, B: Monoid](va: View[V, O, A])(f: A => B): B = va match {
        case Var(_)      => mzero[B]
        case Abs(vs, a)  => f(a)
        case App(op, as) => as foldMap f
      }
    }

  implicit def viewFunctor[V, O]: Functor[View[V, O, ?]] =
    new Functor[View[V, O, ?]] {
      def map[A, B](va: View[V, O, A])(f: A => B): View[V, O, B] = va match {
        case Var(v)      => Var(v)
        case Abs(vs, a)  => Abs(vs, f(a))
        case App(op, as) => App(op, as map f)
      }
    }
}
