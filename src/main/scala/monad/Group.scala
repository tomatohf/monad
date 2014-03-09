package monad

trait SemiGroup[T] {
	// closure
	def operator(x: T, y: T): T
	def associativity_?(x: T, y: T, z: T) =
		operator(operator(x, y), z) == operator(x, operator(y, z))
}
class AdditionSemiGroup[T: Numeric] extends SemiGroup[T] {
	def operator(x: T, y: T) = implicitly[Numeric[T]].plus(x, y)
}
class MultiplicationSemiGroup[T: Numeric] extends SemiGroup[T] {
	def operator(x: T, y: T) = implicitly[Numeric[T]].times(x, y)
}
trait StringSemiGroup extends SemiGroup[String] {
	def operator(x: String, y: String) = x + y
}
trait ListSemiGroup[T] extends SemiGroup[List[T]] {
	def operator(x: List[T], y: List[T]) = x ::: y
}


trait Monoid[T] extends SemiGroup[T] {
	def identity: T
	def identity_?(x: T) = operator(identity, x) == x
}
class AdditionMonoid[T: Numeric] extends AdditionSemiGroup[T] with Monoid[T] {
	val identity = implicitly[Numeric[T]].zero
}
class MultiplicationMonoid[T: Numeric] extends MultiplicationSemiGroup[T] with Monoid[T] {
	val identity = implicitly[Numeric[T]].one
}
trait StringMonoid extends StringSemiGroup with Monoid[String] {
	val identity = ""
}
trait ListMonoid[T] extends ListSemiGroup[T] with Monoid[List[T]] {
	val identity = Nil
}


trait Group[T] extends Monoid[T] {
	def inverse(x: T): T
	def inverse_?(x: T) = operator(x, inverse(x)) == identity
}
class AdditionGroup[T: Numeric] extends AdditionMonoid[T] with Group[T] {
	def inverse(x: T) = implicitly[Numeric[T]].minus(identity, x)
}
