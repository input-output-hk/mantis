package io.iohk.ethereum

import java.io.{File, FileInputStream, FileOutputStream}
import java.net.URL
import java.nio.file._
import java.security.{DigestInputStream, MessageDigest}
import java.util.zip.ZipInputStream

import io.iohk.ethereum.utils.Logger
import org.bouncycastle.util.encoders.Hex

/**
  * A facility to
  * - check the download location for a minimum amount of free space
  * - download a zip from a URL and generate SHA-512 checksum
  * - check the checksum
  * - clean files out of given location
  * - unzip to a given location
  */
object BootstrapDownload extends Logger {

  val bufferSize = 4 * 1024
  val leveldbFolderName = "leveldb"

  private def assertAndLog(cond: Boolean, msg: String): Unit = {
    if (!cond) log.info(msg)
    assert(cond, msg)
  }

  def cleanOutFolder(pathToDownloadTo: Path): Unit = {
    val leveldbFolder = pathToDownloadTo.toFile
    assertAndLog(leveldbFolder.isDirectory, s"${pathToDownloadTo} must be a folder.")
    assertAndLog(
      leveldbFolder.getName == leveldbFolderName,
      s"${pathToDownloadTo} must end in a folder named $leveldbFolderName"
    )
    leveldbFolder.listFiles(pathname => !pathname.getName.endsWith(".zip")).foreach(_.delete())
  }

  def downloadFile(urlToDownloadFrom: String, outFile: File): String = {

    val sha512 = MessageDigest.getInstance("SHA-512")
    val dis = new DigestInputStream(new URL(urlToDownloadFrom).openStream(), sha512)

    try {
      val out = new FileOutputStream(outFile)
      try {
        val buffer = new Array[Byte](bufferSize)

        Iterator.continually(dis.read(buffer)).takeWhile(_ != -1).foreach(out.write(buffer, 0, _))
      } finally (out.close())
      Hex.toHexString(sha512.digest)

    } finally {
      dis.close()
    }
  }

  def unzip(zipFile: File, destination: Path): Unit = {

    val in = new FileInputStream(zipFile)
    try {
      val zis = new ZipInputStream(in)
      try {
        Iterator.continually(zis.getNextEntry).takeWhile(_ != null).foreach { file =>
          if (!file.isDirectory) {
            val outPath = destination.resolve(file.getName)
            val outPathParent = outPath.getParent
            if (!outPathParent.toFile.exists()) {
              outPathParent.toFile.mkdirs()
            }

            val outFile = outPath.toFile
            val out = new FileOutputStream(outFile)
            try {
              val buffer = new Array[Byte](bufferSize)
              Iterator.continually(zis.read(buffer)).takeWhile(_ != -1).foreach(out.write(buffer, 0, _))
            } finally (out.close())
          }
        }
      } finally (zis.close())
    } finally (in.close())
  }

  def deleteDownloadedFile(downloadedFile: File): Unit = {
    if (downloadedFile.delete()) log.info(s"Downloaded file $downloadedFile successfully deleted")
    else log.info(s"Failed to delete downloaded file $downloadedFile")
  }

  // scalastyle:off method.length
  def main(args: Array[String]): Unit = {
    //download a zip file from a url.

    assertAndLog(
      args.length == 4,
      "Provide the url to download from, " +
        " expected hash of the downloaded file, " +
        " the minimum required free disk space in giga bytes" +
        " and the path to extract the file to"
    )

    val urlToDownloadFrom = new URL(args(0))
    val expectedHash = args(1)
    val minimumExpectedDiskSpace = args(2)
    val pathToDownloadTo = Paths.get(args(3))

    val bytesInOneGigaByte = 1024L * 1024L * 1024L
    val minimumExpectedDiskSpaceInBytes = minimumExpectedDiskSpace.toLong * bytesInOneGigaByte

    val urlToDownloadFromAsFile = new File(urlToDownloadFrom.getFile)
    val pathToDownloadToAsFile = pathToDownloadTo.toFile
    val downloadedFile = new File(pathToDownloadToAsFile, urlToDownloadFromAsFile.getName)

    log.info(s"Running Bootstrap download ... ")
    log.info(s"Expected Minimum disk space is $minimumExpectedDiskSpace GB")
    log.info(s"Download path is $urlToDownloadFrom")
    log.info(s"Path to download to is $pathToDownloadTo")

    if (!pathToDownloadToAsFile.exists()) pathToDownloadToAsFile.mkdirs()

    assertAndLog(pathToDownloadToAsFile.isDirectory, s"$pathToDownloadToAsFile must be a folder.")
    assertAndLog(
      pathToDownloadToAsFile.getUsableSpace() >= minimumExpectedDiskSpaceInBytes,
      s"There is not enough free space ($minimumExpectedDiskSpace GB) to download and expand to $pathToDownloadTo "
    )

    log.info(s"Free space check ok, starting download! (this could take some time)")
    val hash = downloadFile(args(0), downloadedFile)

    log.info(s"Download complete, checking hash against $expectedHash ...")
    assertAndLog(hash == expectedHash, s"The zip file hash $hash did NOT match the expected hash $expectedHash")

    log.info(s"Hash OK, clean out folder...")
    cleanOutFolder(pathToDownloadTo)

    log.info(s"Unzip file ${pathToDownloadTo} ${downloadedFile}...")
    unzip(downloadedFile, pathToDownloadTo)

    deleteDownloadedFile(downloadedFile)

    log.info(s"Bootstrap download successful.")

  }
}
