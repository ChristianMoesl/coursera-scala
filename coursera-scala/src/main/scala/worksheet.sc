val a = Stream(1,2,3,4,5,6)

def append(a: Stream[Int]): Stream[Int] = {
  for {
    elem <- a
    x <- elem to 6
  } yield x
}

println(append(a))