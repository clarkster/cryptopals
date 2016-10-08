package clarkster.challenges

import org.reflections.Reflections

import scala.collection.JavaConversions._
import scala.reflect.runtime.universe

object Runner {

  val runtimeMirror = universe.runtimeMirror(getClass.getClassLoader)

  def loadFromClassName(clazz: Class[_ <: Challenge]) : Challenge = {
    val module = runtimeMirror.staticModule(clazz.getName)
    val obj = runtimeMirror.reflectModule(module)
    obj.instance.asInstanceOf[Challenge]
  }

  def challenges: List[Challenge] = {
    val reflections = new Reflections("clarkster.challenges")
    val subclasses = reflections.getSubTypesOf(classOf[Challenge])
    val x : List[Challenge] = subclasses.toList.map(loadFromClassName)
    x.sortBy(_.number)
  }

  def main(args: Array[String]): Unit = {
    challenges.foreach{challenge =>
      println("============")
      println("Challenge " + challenge.number)
      println("============")
      println(challenge.desc)
      println("============")
      challenge.main(args)
      println()
    }
  }
}
