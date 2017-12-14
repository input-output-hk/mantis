package io.iohk.ethereum


import java.io.{File, FileInputStream, FileOutputStream}
import java.net.URL
import java.nio.file._
import java.security.{DigestInputStream, MessageDigest}
import java.util.zip.ZipInputStream

import io.iohk.ethereum.utils.Logger


/**
  * A facility to
  * - check the download location for a minimum amount of free space
  * - download a zip from a URL
  * - check the checksum
  * - unzip to a given location
  */
object BootstrapDownload extends Logger {

  def downloadFile(urlToDownloadFrom: String, outFile: File): String = {

    val md5 = MessageDigest.getInstance("MD5")
    val is = new URL(urlToDownloadFrom).openStream()
    try {
      val dis = new DigestInputStream(is, md5)
      try {
        val out = new FileOutputStream(outFile)
        try {
          val buffer = new Array[Byte](8192)
          Stream.continually(dis.read(buffer)).takeWhile(_ != -1).foreach(out.write(buffer, 0, _))
        } finally (out.close())
        md5.digest.map("%02x".format(_)).mkString
      } finally(dis.close())
    } finally(is.close())
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

    val urlToDownloadFrom = new URL(args(0))

    val pathToDownloadTo = Paths.get(args(3))
    val urlToDownloadFromAsFile = new File(urlToDownloadFrom.getFile)
    val downloadedFileNameAsFile = new File(urlToDownloadFromAsFile.getName)

    log.info(s"Running Bootstrap download ... ")
    log.info(s"Expected Minimum disk space is $minimumExpectedDiskSpace ")
    log.info(s"Download path is $urlToDownloadFrom")
    log.info(s"Path to download to is $pathToDownloadTo")

    assert(pathToDownloadTo.toFile.getUsableSpace() >= minimumExpectedDiskSpaceInBytes,
      s"There is not enough free space ($minimumExpectedDiskSpace GB) to download and expand to $pathToDownloadTo ")

    log.info(s"Free space check ok, starting download! (this could take some time)")
    val hash = downloadFile(args(0), downloadedFileNameAsFile)

    log.info(s"Download complete, checking hash against $expectedHash ...")
    assert(hash == expectedHash, s"The zip file hash $hash did NOT match the expected hash $expectedHash")

    log.info(s"Hash OK, clean out folder...")
    //TODO
    
    log.info(s"Unzip file ${pathToDownloadTo}${downloadedFileNameAsFile}...")
    unzip(downloadedFileNameAsFile, pathToDownloadTo)

    log.info(s"Bootstrap download successful.")

  }
}
