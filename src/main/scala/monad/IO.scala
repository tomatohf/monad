package monad

import scala.language.existentials

sealed trait IOState {
	def change: IOState
}


sealed trait IOAction[A] extends Function1[IOState, (IOState, A)] {
	def map[B](f: A => B): IOAction[B] = IOMonad.map(f)(this)
	def flatMap[B](f: A => IOAction[B]) = IOMonad.flatMap(f)(this)
	def orElse[B](alternative: IOAction[B]) = IOMonad.operator(this, alternative)
}
object IOAction {
	def apply[T](t: => T) = new IOAction[T] {
		def apply(state: IOState) = (state.change, t)
	}

	val error = IOMonad.identity

	def print[T](t: T) = apply(Console.print(t))
	def println[T](t: T) = apply(Console.println(t))
	val readLine = apply(Console.readLine)
}


object IOMonad extends Monad[IOAction] {
	def unit[T](t: T) = new IOAction[T] {
		def apply(state: IOState) = (state, t)
	}
	
	def flatMap[A, B](f: A => IOAction[B]) = action => new IOAction[B] {
		def apply(state: IOState) = {
			val (s, result) = action(state)
			f(result)(s)
		}
	}

	val identity = new IOAction[Nothing] {
		def apply(state: IOState) = ???
	}

	def operator(x: IOAction[_], y: IOAction[_]) = new IOAction[T forSome { type T }] {
		def apply(state: IOState) = try {
			x(state)
		}
		catch {
			case _: Throwable => y(state)
		}
	}
}


trait IOApp extends App {
	def mainAction: IOAction[T forSome { type T }]

	private class DefaultState(id: Int) extends IOState {
		def change = new DefaultState(id + 1)
	}

	mainAction(new DefaultState(1))
}


object IO extends IOApp {
	def mainAction = {
		for {
			_ <- IOAction.print("I am IO monad.")
			_ <- IOAction.print(" ^_^ ")
			_ <- IOAction.println("What's your name ?")
			name <- IOAction.readLine
			_ <- name match {
				case "tomato" => IOAction.println("Hello Tomato !")
				case "hefan" => IOAction.println("Ni Hao hefan !")
				case _ => IOAction.error
			}
		} yield ()
	}.orElse(IOAction.println("I don't know you ..."))
}
