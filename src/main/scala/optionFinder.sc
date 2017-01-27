  trait OptionFinder[A] {
    def isOption(value: A): Boolean
  }
  def createOptionFinder[A](func: A => Boolean): OptionFinder[A] =
    new OptionFinder[A] {
      def isOption(value: A) = func(value)
    }
  implicit def missFinder[A]: OptionFinder[A] = createOptionFinder(_ => false)
  implicit def hitFinder[A]: OptionFinder[Option[A]] = createOptionFinder(_ => true)

  //trait HorseFinder[A]{
  //  def isHorse: Boolean
  //}
  case class HorseFinder[A](isTrue: Boolean)

  implicit def missHorse[A]: HorseFinder[A] = HorseFinder(false)
  implicit def hitHorse[A]: HorseFinder[Option[A]] = HorseFinder(true)

  def test[A](value: A)(implicit optionFinder: OptionFinder[A], hf: HorseFinder[A]): String = {
    //if (optionFinder.isOption(value)) { "Option"} else { "Something else"}
    if (hf.isTrue) { "Found Option"} else { "Found something else"}
  }
  val a: Option[String] = Some("sfjal")
  val b: Option[List[Int]] = None
  val x = test(a)
  val x2 = test(Some("d"))
  val x3 = test(None)
  val y = test(4)
  val z = test(b)
