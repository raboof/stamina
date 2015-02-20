
package object stamina {
  /** Type alias for Akka's ByteSttring so we don't have to import it everywhere. */
  type ByteString = akka.util.ByteString
  val ByteString = akka.util.ByteString

  implicit def nonEmptyStringToKey(in: String): Key = macro KeyMacro.nonEmptyStringToKey
  implicit def keytoString(key: Key): String = key.value
}

package stamina {
  /**
   * Marker trait for classes that should be persisted using the StaminaAkkaSerializer.
   *
   * This marker trait can be used to mark all your top-level persistable classes
   * (i.e. events, snapshots, etc.) so that you will only need a few lines of
   * configuration in your application.conf, namely:
   *
   * akka.actor.serializers.stamina = <FQCN of your subclass of StaminaAkkaSerializer>
   * akka.actor.serialization-bindings {
   *   "stamina.Persistable" = stamina
   * }
   */
  trait Persistable extends java.io.Serializable

  case class Key(value: String) {
    def getUtf8Bytes: Array[Byte] = value.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    override def toString = value
  }

  object KeyMacro {
    import scala.reflect.macros.Context

    def nonEmptyStringToKey(c: Context)(in: c.Expr[String]): c.Expr[Key] = {
      import c.universe._
      val q"${ s: String }" = q"${in}"
      if (s.isEmpty) c.abort(c.enclosingPosition, s"Persistence keys must be non-empty!")
      else c.Expr[Key](q"Key($in)")
    }
  }

  // TODO: the value must be a literal too! Otherwise the macro won't work!
  // def ensureValidStringLiteral(c: Context)(value: c.Expr[String], notValidMsg: String, notLiteralMsg: String)(isValid: String => Boolean): Unit = {
  //   import c.universe._

  //   value.tree match {
  //     case Literal(stringConst) =>
  //       val literalValue = stringConst.value.toString
  //       if (!isValid(literalValue))
  //         c.abort(c.enclosingPosition, notValidMsg)
  //     case _ =>
  //       c.abort(c.enclosingPosition, notLiteralMsg)
  //   }
  // }

  import scala.util.control._

  case class UnregisteredTypeException(obj: AnyRef)
    extends RuntimeException(s"No persister registered for class: ${obj.getClass}")
    with NoStackTrace

  case class UnsupportedDataException(persisted: Persisted)
    extends RuntimeException(s"No unpersister registered for key: '${persisted.key}' and version: ${persisted.version}")
    with NoStackTrace

  case class UnrecoverableDataException(persisted: Persisted, error: Throwable)
    extends RuntimeException(s"Error while trying to unpersist data with key '${persisted.key}' and version ${persisted.version}. Cause: ${error}")
    with NoStackTrace
}
