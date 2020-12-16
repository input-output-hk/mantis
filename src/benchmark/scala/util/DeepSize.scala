package util

import java.lang.reflect.Modifier
import java.util

import scala.collection.mutable


object DeepSize {
  private val SKIP_POOLED_OBJECTS: Boolean = false

  private def isPooled(paramObject: AnyRef): Boolean = {
    paramObject match{
      case e: java.lang.Enum[_]   => true
      case s: java.lang.String    => s eq s.intern()
      case b: java.lang.Boolean   => (b eq java.lang.Boolean.TRUE) || (b eq java.lang.Boolean.FALSE)
      case i: java.lang.Integer   => i eq java.lang.Integer.valueOf(i)
      case s: java.lang.Short     => s eq java.lang.Short.valueOf(s)
      case b: java.lang.Byte      => b eq java.lang.Byte.valueOf(b)
      case l: java.lang.Long      => l eq java.lang.Long.valueOf(l)
      case c: java.lang.Character => c eq java.lang.Character.valueOf(c)
      case _ => false
    }
  }

  /**
    * Calculates deep size
    *
    * @param obj
    * object to calculate size of
    * @return object deep size
    */
  def apply(obj: AnyRef): Long = {
    deepSizeOf(obj)
  }

  private def skipObject(obj: AnyRef, previouslyVisited: util.Map[AnyRef, AnyRef]): Boolean = {
    if (SKIP_POOLED_OBJECTS && isPooled(obj)) {
      true
    } else {
      (obj == null) || previouslyVisited.containsKey(obj)
    }
  }

  private def deepSizeOf(obj0: AnyRef): Long = {
    val previouslyVisited = new util.IdentityHashMap[AnyRef, AnyRef]
    val objectQueue = mutable.Queue(obj0)
    var current = 0L
    while(objectQueue.nonEmpty){
      val obj = objectQueue.dequeue()
      if (!skipObject(obj, previouslyVisited)){
        previouslyVisited.put(obj, null)
        val thisSize = agent.Agent.getObjectSize(obj)

        // get size of object + primitive variables + member pointers
        // for array header + len + if primitive total value for primitives
        obj.getClass match{
          case a if a.isArray =>
            current += thisSize
            // primitive type arrays has length two, skip them (they included in the shallow size)
            if (a.getName.length != 2) {
              val lengthOfArray = java.lang.reflect.Array.getLength(obj)
              for (i <- 0 until lengthOfArray) {
                objectQueue.enqueue(java.lang.reflect.Array.get(obj, i))
              }
            }
          case c =>
            current += thisSize
            var currentClass: Class[_] = c
            do {
              val objFields = currentClass.getDeclaredFields
              for(field <- objFields) {
                if (
                  !Modifier.isStatic(field.getModifiers) &&
                    !field.getType.isPrimitive
                ) {
                  field.setAccessible(true)
                  var tempObject: AnyRef = null
                  tempObject = field.get(obj)
                  if (tempObject != null) objectQueue.enqueue(tempObject)
                }
              }
              currentClass = currentClass.getSuperclass
            } while (currentClass != null)

        }

      }
    }
    current
  }
}