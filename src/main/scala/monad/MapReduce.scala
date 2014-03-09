package monad

object MapReduce {
	def reduce[T: Monoid](xs: Seq[T]) = {
		val monoid = implicitly[Monoid[T]]
		xs.foldLeft(monoid.identity)(monoid.operator)
	}

	implicit object StringMonoid extends StringMonoid
	implicit object IntMonoid extends AdditionMonoid[Int]

	def main(args: Array[String]) {
		println(reduce(args))

		def map = args.map(_.map(_.toInt))
		println(reduce(map.map(reduce(_))))
	}
}
