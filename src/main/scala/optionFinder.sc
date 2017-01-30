/*
  trait OptionFinder[A] {
    def isOption(value: A): Boolean
  }
  def createOptionFinder[A](func: A => Boolean): OptionFinder[A] =
    new OptionFinder[A] {
      def isOption(value: A) = func(value)
    }
  implicit def missFinder[A]: OptionFinder[A] = createOptionFinder(_ => false)
  implicit def hitFinder[A]: OptionFinder[Option[A]] = createOptionFinder(_ => true)
  */

  //trait HorseFinder[A]{
  //  def isHorse: Boolean
  //}
  case class OptionFinder[A](isOption: Boolean)

  implicit def notOption[A]: OptionFinder[A] = OptionFinder(false)
  //implicit def hitOption[A]: OptionFinder[Option[A]] = OptionFinder(true)

  def myFunction[A](value: A)(implicit optionFinder: OptionFinder[A]): String = {
    if (optionFinder.isOption) {"Found Option"} else {"Found something else"}
  }
  val x1: Option[String] = Some("sfjal")
  val x2: Option[List[Int]] = None
  myFunction(None:Option[Double])
  val x = myFunction(x2)
  val x4 = myFunction(Some("d"))
  val x3 = myFunction(None)
  val y = myFunction(4)
  val z = myFunction(x3)
