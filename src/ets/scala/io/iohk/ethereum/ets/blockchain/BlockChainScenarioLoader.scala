package io.iohk.ethereum.ets.blockchain

import java.io.File

import io.iohk.ethereum.ets.vm.ScenarioLoader.{getClass, log}
import io.iohk.ethereum.ets.vm.{ScenarioGroup, ScenarioParser, TestOptions}
import io.iohk.ethereum.utils.Logger
import org.apache.commons.io.FileUtils
import scala.collection.JavaConverters._
import scala.io.Source
import io.circe._, io.circe.parser._

object BlockChainScenarioLoader extends Logger {

  val unsupportedNetworks = Set("Byzantium","Constantinople")


  def load(options: TestOptions): List[BlockChainScenarioGroup] = {
    val testDir = new File(getClass.getClassLoader.getResource("ets/BlockchainTests").toURI)
    val files = FileUtils.listFiles(testDir, Array("json"), true).asScala.toList

    files.flatMap { file =>
      val name = file.getAbsolutePath.drop(testDir.getAbsolutePath.length + 1).dropRight(".json".length)

      if (!options.isGroupIncluded(name))
        None
      else {
        log.debug(s"Loading test scenarios from: $file")
        val text = Source.fromFile(file).getLines.mkString
        val scenarios = BlockChainScenarioParser.parse(text).filter( scenarioGroup => !unsupportedNetworks.contains(scenarioGroup._2.network))
        Some(BlockChainScenarioGroup(name, scenarios))
      }
    }
  }
}