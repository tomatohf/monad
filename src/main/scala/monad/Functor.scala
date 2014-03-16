package monad

import scala.language.higherKinds

trait Functor[F[_]] {
	// fmap
	def map[A, B](f: A => B): F[A] => F[B]
}
/*
 * Assuming that all types and morphisms(functions) are in the Hask category,
 * all functors are endofunctors(map Hask to Hask)
 */
trait EndoFunctor[F[_]] extends Functor[F]
trait IdentityFunctor extends EndoFunctor[({ type Identity[T] = T })#Identity] {
	def map[A, B](f: A => B) = f
}


trait ListFunctor[T] extends EndoFunctor[List] {
	def map[A, B](f: A => B) = _.map(f)
}
trait OptionFunctor extends EndoFunctor[Option] {
	def map[A, B](f: A => B) = _.map(f)
}


trait EndoFunction[T] extends Function1[T, T]
trait IdentityFunction[T] extends EndoFunction[T] {
	def apply(t: T) = t
}
