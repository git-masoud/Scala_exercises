//write flatten:

flatten(List(List(1, 1), 2, List(3, List(5, 8))))


def flatten[B](x: List[Any]): List[B] = x match {
  case Nil => Nil
  case h :: tail => h match {
    case y: List[B] => flatten(y) ::: flatten(tail)
    case z: B => z :: flatten(tail)
  }
}

//-----------------------------------------------------------------------------------
//Eliminate consecutive duplicates:

//  Replace repeated elements with one element. The order should be preserved.

compress(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))

//res0: List[String] = List("a", "b", "c", "a", "d", "e")

def compress[A](lst: List[A]): List[A] = lst match {
  case Nil => Nil
  case x :: tail => if (tail.size == 0) List(x) else if (x != tail(0)) x :: compress(tail) else compress(tail)
}


//-----------------------------------------------------------------------------------
//Pack consecutive duplicates of list elements into sublists.

//If a list contains repeated elements they should be placed in separate sublists.

pack(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))

//res0: List[List[String]] = List(List("a", "a", "a", "a"), List("b"), List("c", "c"), List("a", "a"), List("d"), List("e", "e", "e", "e"))

def pack[A](lst: List[A],tempList: List[A]=List()): List[List[A]] = lst match {
  case Nil => Nil
  case x :: tail => if (tail.size == 0 || x != tail(0)) (x::tempList) :: pack(tail)
  else pack(tail, x::tempList)

}

//-----------------------------------------------------------------------------------
//Run-length encoding of a list:

//Run-length encoding data: Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.

encode(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))

//res0: List[(Int, String)] = List((4,"a"), (1,"b"), (2,"c"), (2,"a"), (1,"d"), (4,"e"))

def encode[A](lst: List[A], counter: Int=1): List[(Int, A)] = lst match {
  case Nil => Nil
  case x :: tail => if (tail.size == 0 || x != tail(0)) ((counter, x)) :: encode(tail)
  else encode(tail, counter + 1)

}

//-----------------------------------------------------------------------------------
//Decode a run-length encoded list.

decode(List((4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e")))

//res0= List[String] = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")

def decode[A](lst: List[(Int, A)]): List[A] = lst match {
  case Nil => Nil
  case x :: tail => if (x._1>1) x._2::decode((x._1-1,x._2)::tail)
  else x._2::decode(tail)

}

//-----------------------------------------------------------------------------------


def map[A,B](lst:List[A], f:A => B):List[B]=lst match{
  case Nil => Nil
  case x::tail => f(x)::map(tail,f)
}

//-----------------------------------------------------------------------------------

def filter[A](lst:List[A], f:A => Boolean):List[A]=lst match{
  case Nil => Nil
  case x::tail => if(f(x)) x :: filter(tail,f) else filter(tail,f)
}
//-----------------------------------------------------------------------------------

def concat[A](right:List[A], left:List[A]):List[A]= right match{
  case Nil => left
  case x::tail => x::concat(tail,left)
}


val myList=List(9,5,3,4,1)

map(myList,(x: Int) => x * x)

filter(myList,(x: Int) => x > 4)

concat(myList,List(3,6,3,23))