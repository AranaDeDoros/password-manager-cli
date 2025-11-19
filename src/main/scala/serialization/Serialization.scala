package org.aranadedoros
package serialization

import java.nio.file.{Files, Paths, StandardOpenOption}

object Serialization {
  def writeBytes(path: String = "usr.enc", data: Array[Byte]): Either[Throwable, Unit] =
    try
      Files.write(
        Paths.get(path),
        data,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
      Right(())
    catch
      case e: Throwable =>
        Left(e)

  def readBytes(path: String = "db.enc"): Either[Throwable, Array[Byte]] =
    try
      val bytes = Files.readAllBytes(Paths.get(path))
      Right(bytes)
    catch
      case e: Throwable =>
        Left(e)

}
