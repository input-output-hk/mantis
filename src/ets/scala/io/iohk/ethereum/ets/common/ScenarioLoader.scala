package io.iohk.ethereum.ets.common

import java.io.File

import io.iohk.ethereum.utils.Logger
import org.apache.commons.io.FileUtils

import scala.jdk.CollectionConverters._
import scala.io.Source


trait ScenarioLoader[T] extends ScenarioParser[T] with Logger {

  def load(path: String, options: TestOptions, ignoredTestNames: Set[String] = Set.empty): List[ScenarioGroup[T]] = {
    val testDir = new File(getClass.getClassLoader.getResource(path).toURI)
    val files = FileUtils.listFiles(testDir, Array("json"), true).asScala.toList

    files.filterNot(file => ignoredTestNames.contains(file.getName)).flatMap { file =>
      val name = file.getAbsolutePath.drop(testDir.getAbsolutePath.length + 1).dropRight(".json".length)

      if (!options.isGroupIncluded(name))
        None
      else {
        log.info(s"Loading test scenarios from: $file")
        val text = Source.fromFile(file).getLines().mkString
        val scenarios = parse(text)
        Some(ScenarioGroup(name, scenarios))
      }
    }
  }
}
