package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  def mergeRecur(left: Seq[Int], right: Seq[Int]): Seq[Int] = (left, right) match {
    case (Nil, _) => right
    case (_, Nil) => left
    case (lH :: lT, rH :: rT) => if (lH < rH) Seq(lH) ++ mergeRecur(lT, right) else Seq(rH) ++ mergeRecur(left, rT)
  }

  def mergeSort(data: Seq[Int]): Seq[Int] = data.length match {
    case l if l <= 1 => data
    case _      =>
      val (left, right) = data.splitAt(data.length / 2)
      val sortedLeft = mergeSort(left)
      val sortedRight = mergeSort(right)
      mergeRecur(sortedLeft, sortedRight)
  }

  println(Seq(4,6,2,1,0,5000,4,-100))
  println(mergeSort(Seq(4,6,2,1,0,5000,4,-100)))

}
