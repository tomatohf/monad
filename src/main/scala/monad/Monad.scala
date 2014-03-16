package monad

import scala.language.{ higherKinds, existentials }

// A monad is an endofunctor, together with two natural transformations(unit and join).
trait MonadTheory[M[_]] extends EndoFunctor[M] {
	// return
	def unit[T](t: T): M[T]
	// flatten
	def join[T](m: M[M[T]]): M[T]

	// bind
	def flatMap[A, B](f: A => M[B]): M[A] => M[B] = ma => join(map(f)(ma))
}


/*
 * Any triple that consists of
 * a type constructor and two functions bind and return
 * that obey certain identities (axioms) define a monad.
 *
 * In category theory this triple is called a Kleisli triple
 * and can be used as an alternative definition of a monad.
 */
trait Monad[M[_]] extends EndoFunctor[M] with Monoid[M[_]] {
	// return
	def unit[T](t: T): M[T]
	// bind
	def flatMap[A, B](f: A => M[B]): M[A] => M[B]

	// fmap
	def map[A, B](f: A => B): M[A] => M[B] = flatMap(a => unit(f(a)))
	// flatten
	def join[T](m: M[M[T]]): M[T] = {
		def identity[T](t: T): T = t
		flatMap[M[T], T](identity)(m)
	}
}


object ListMonad extends Monad[List] with ListMonoid[X forSome { type X }] {
	def unit[T](t: T) = List(t)
	def flatMap[A, B](f: A => List[B]) = _.flatMap(f)
}


object ContainerMonad extends Monad[Container] {
	def unit[T](t: T) = if (t == null) identity else FullContainer(t)
	def flatMap[A, B](f: A => Container[B]) = _ match {
		case FullContainer(content) => f(content)
		case _ => identity
	}
	val identity = EmptyContainer
	def operator(x: Container[_], y: Container[_]) = x match {
		case FullContainer(_) => x
		case _ => y
	}
}
sealed trait Container[+A] {
	def map[B](f: A => B): Container[B] = ContainerMonad.map(f)(this)
	def flatMap[B](f: A => Container[B]) = ContainerMonad.flatMap(f)(this)
	def orElse[B](alternative: Container[B]) = ContainerMonad.operator(this, alternative)
}
final case class FullContainer[A](content: A) extends Container[A]
case object EmptyContainer extends Container[Nothing]
