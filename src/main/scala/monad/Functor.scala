package monad

import scala.language.higherKinds

trait Functor[/* lift type */M[_]] {
	// lift morphism/function
	def lift[A, B](f: A => B): M[A] => M[B]
}
trait ListFunctor extends Functor[List] {
	def lift[A, B](f: A => B) = _.map(f)
}
trait OptionFunctor extends Functor[Option] {
	def lift[A, B](f: A => B) = _.map(f)
}


trait Container[X] extends Functor[Container] {
	def map[Y](f: X => Y): Container[Y]
	def lift[A, B](f: A => B) = _.map(f)
}
case class NaiveContainer[X](x: X) extends Container[X] {
	def map[Y](f: X => Y) = NaiveContainer(f(x))
}


trait EndoFunction[T] extends Function1[T, T]
trait IdentityFunction[T] extends EndoFunction[T] {
	def apply(t: T) = t
}


trait EndoFunctor extends Functor[({ type Identity[T] = T })#Identity]
trait IdentityFunctor extends EndoFunctor {
	def lift[A, B](f: A => B) = f
}
