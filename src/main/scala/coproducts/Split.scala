package coproducts

import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil}

/*
Suppose we have a sealed trait and few implementations:
sealed trait Animal
case class Cat(a: Int) extends Animal
case class Dog(b: Int) extends Animal
case class Fox(c: Int) extends Animal

and a list:
val animals: List[Animal]

how to split the list into sub-lists per a subclass?
val cats: List[Cat] = ???
val dogs: List[Dog] = ???
val foxes: List[Fox] = ???

Of course it must work w/o boilerplate for arbitrary numbers of children
 */
object Split {

  trait Splitter[T <: Coproduct] {
    type R <: HList

    def split(list: List[T]): R

  }

  type Aux[T <: Coproduct, R0 <: HList] = Splitter[T] {
    type R = R0
  }

  implicit val cNilSplitter = new Splitter[CNil] {
    type R = HNil

    override def split(list: List[CNil]): HNil = HNil
  }

  implicit def cPlusSplitter[H, T <: Coproduct, R1 <: HList](implicit ev: Aux[T, R1]): Aux[H :+: T, List[H] :: R1] = new Splitter[H :+: T] {
    type R = List[H] :: R1

    override def split(list: List[H :+: T]): ::[List[H], ev.R] = {
      val heads: List[H] = list.flatMap(e => e.eliminate(h => Some(h), t => None))
      val tails: List[T] = list.flatMap(e => e.eliminate(h => None, t => Some(t)))
      val sub: R1 = ev.split(tails)
      heads :: sub
    }
  }

  def splitCoproduct[T <: Coproduct, R <: HList](list: List[T])(implicit ev: Aux[T, R]): R = ev.split(list)

  def split[X, T <: Coproduct, R <: HList](list: List[X])(implicit gen: Generic.Aux[X, T], ev: Aux[T, R]): R = {
    val asCoproduct: List[T] = list.map(gen.to)
    splitCoproduct[T, R](asCoproduct)(ev)
  }

}


object Runner {

  import Split._

  def main(args: Array[String]): Unit = {

    sealed trait Animal
    case class Cat(a: Int) extends Animal
    case class Dog(b: Int) extends Animal
    case class Fox(c: Int) extends Animal

    val animals: List[Animal] = List(Cat(1), Dog(1), Cat(2), Fox(1), Dog(2), Dog(3))
    val result = split(animals) //List[Cat] :: List[Dog] :: List[Fox] :: HNil
    println(result)
    val cats: List[Cat] = result.head
    val dogs: List[Dog] = result.tail.head
    val foxes: List[Fox] = result.tail.tail.head
    println(cats)
    println(dogs)
    println(foxes)
  }
}