package clarkster.challenges


trait ReflectionSugars{
  import scala.reflect.runtime.{universe => ru}
  private lazy val universeMirror = ru.runtimeMirror(getClass.getClassLoader)

  def companionOf[T](implicit tt: ru.TypeTag[T])  = {
    val companionMirror = universeMirror.reflectModule(ru.typeOf[T].typeSymbol.companionSymbol.asModule)
    companionMirror.instance
  }

}

trait Challenge extends ReflectionSugars {
  val number : Int
  val desc : String
  def main(args: Array[String]): Unit

  def companion = companionOf[Challenge]
}
