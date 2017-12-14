package io.iohk.ethereum


import java.io.{File, FileInputStream, FileOutputStream}
import java.net.URL
import java.nio.file._
import java.security.{DigestInputStream, MessageDigest}
import java.util.zip.ZipInputStream

import scala.language.postfixOps
import scala.sys.process._
import io.iohk.ethereum.utils.Logger

/**
  * A facility to
  * - check the download location for a minimum amount of free space
  * - download a zip from a URL
  * - check the checksum
  * - unzip to a given location
  */
object BootstrapDownload  {

  object log {
    def info(a: String) = println(a)
  }

  // Compute a hash of a file
  // The output of this function should match the output of running "md5sum <file>"
  def computeHash(file: File): String = {

    val buffer = new Array[Byte](8192)
    val md5 = MessageDigest.getInstance("MD5")

    val is = new FileInputStream(file)
    try {
      val dis = new DigestInputStream(is, md5)
      try {
        while (dis.read(buffer) != -1) {}
      } finally {
        dis.close()
      }
    } finally(is.close())
    md5.digest.map("%02x".format(_)).mkString
  }

  def downloadFile(url: String, filename: String) = {
    new URL(url) #> new File(filename) !!
  }

  def unzip(zipFile: File, destination: Path): Unit = {

    val in = new FileInputStream(zipFile)
    try {
      val zis = new ZipInputStream(in)
      try {
        Stream.continually(zis.getNextEntry).takeWhile(_ != null).foreach { file =>
          if (!file.isDirectory) {
            val outPath = destination.resolve(file.getName)
            val outPathParent = outPath.getParent
            if (!outPathParent.toFile.exists()) {
              outPathParent.toFile.mkdirs()
            }


            val outFile = outPath.toFile
            val out = new FileOutputStream(outFile)
            try {
              val buffer = new Array[Byte](8192)
              Stream.continually(zis.read(buffer)).takeWhile(_ != -1).foreach(out.write(buffer, 0, _))
            } finally(out.close())
          }
        }
      } finally(zis.close())
    } finally(in.close())
  }

  def main(args: Array[String]): Unit = {
    //download a zip file from a url.

    assert(args.length == 4, "Provide the url to download from, " +
      " expected hash of the downloaded file, " +
      " the minimum required free disk space in giga bytes" +
      " and the path to extract the file to")

    val minimumExpectedDiskSpace = args(2)
    val bytesInOneGigaByte = 1073741824l
    val minimumExpectedDiskSpaceInBytes =  minimumExpectedDiskSpace.toLong * bytesInOneGigaByte
    val expectedHash = args(1)
    val path = Paths.get(args(0))
    val pathToDownloadTo = Paths.get(args(3))
    val f = path.getFileName.toFile

    log.info(s"Running Bootstrap download ... ")
    log.info(s"Expected Minimum disk space is $minimumExpectedDiskSpace ")
    log.info(s"Download path is $path")
    log.info(s"Path to download to is $pathToDownloadTo")

    assert(pathToDownloadTo.toFile.getUsableSpace() >= minimumExpectedDiskSpaceInBytes,
      s"There is not enough free space ($minimumExpectedDiskSpace GB) to download and expand to $pathToDownloadTo ")

    log.info(s"Free space check ok, starting download! (this could take some time)")
    downloadFile(args(0), path.getFileName.toString)

    log.info(s"Download complete, checking hash against $expectedHash ...")
    val hash = computeHash(f)

    assert(hash == expectedHash, s"The zip file hash $hash did NOT match the expected hash $expectedHash")

    log.info(s"Hash OK, unzipping file...")
    unzip(f, pathToDownloadTo)

    log.info(s"Bootstrap download successful.")

  }
}
