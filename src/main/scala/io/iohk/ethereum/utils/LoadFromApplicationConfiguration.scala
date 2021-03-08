package io.iohk.ethereum.utils

import ch.qos.logback.core.joran.action.Action
import ch.qos.logback.core.joran.spi.InterpretationContext
import com.typesafe.config.ConfigFactory
import org.xml.sax.Attributes

/** Make properties defined in application.conf available to logback
  */
class LoadFromApplicationConfiguration extends Action {

  val config = ConfigFactory.load
  override def begin(ic: InterpretationContext, body: String, attributes: Attributes): Unit = {
    ic.addSubstitutionProperty(attributes.getValue("as"), config.getString(attributes.getValue("key")))
  }
  override def end(ic: InterpretationContext, body: String): Unit = ()
}
