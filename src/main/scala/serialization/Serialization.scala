package org.aranadedoros
package serialization

import auth.Security.KeyProvider.given_SecretKey
import auth.Security.{Crypto, EncryptedPassword}
import model.Model.{Database, Entry, EntryKey, Site}

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}

import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.collection.immutable.HashMap

object JsonSerialization {

  given Encoder[Site] = Encoder.encodeString.contramap(_.value)
  given Decoder[Site] = Decoder.decodeString.emap { s =>
    Site.fromRaw(s).left.map(identity)
  }

  given Encoder[EntryKey] = Encoder.encodeString.contramap(_.value)
  given Decoder[EntryKey] = Decoder.decodeString.emap { s =>
    EntryKey.fromRaw(s).left.map(err => err)
  }

  given Encoder[EncryptedPassword] =
    Encoder.encodeString.contramap { enc =>
      java.util.Base64.getEncoder.encodeToString(enc.bytes)
    }

  given Decoder[EncryptedPassword] =
    Decoder.decodeString.map { base64 =>
      val bytes = java.util.Base64.getDecoder.decode(base64)
      EncryptedPassword(bytes)
    }

  private case class EntryRecord(key: EntryKey, entry: Entry)
  private given Encoder[EntryRecord] = deriveEncoder
  private given Decoder[EntryRecord] = deriveDecoder

  given Encoder[Entry] = deriveEncoder
  given Decoder[Entry] = deriveDecoder

  given Encoder[Database] = Encoder.instance { db =>
    Json.obj(
      "name" -> Json.fromString(db.name),
      "entries" ->
        Json.fromValues(
          db.entries.map { case (k, e) =>
            EntryRecord(k, e).asJson
          }
        )
    )
  }

  given Decoder[Database] = Decoder.instance { cursor =>
    for
      name <- cursor.get[String]("name")
      list <- cursor.get[List[EntryRecord]]("entries")
    yield Database(name, HashMap.from(list.map(r => r.key -> r.entry)))
  }

  def serialize(db: Database): Either[Throwable, Array[Byte]] =
    try
      val jsonString = db.asJson.noSpaces
      val raw        = jsonString.getBytes("UTF-8")
      val encrypted  = Crypto.encrypt(raw)
      Right(encrypted)
    catch
      case e: Throwable => Left(e)

  private def deserialize(bytes: Array[Byte]): Either[Throwable, Database] =
    try
      val decryptedBytes = Crypto.decrypt(bytes)
      val json           = new String(decryptedBytes, "UTF-8")
      io.circe.parser.decode[Database](json).left.map(err => err)
    catch
      case e: Throwable => Left(e)

  def writeDatabase(db: Database, path: String = "db.enc"): Either[Throwable, Unit] =
    for
      encrypted <- serialize(db)
      _         <- BinarySerialization.writeBytes(encrypted)
    yield ()

  def readDatabase(path: String = "db.enc"): Either[Throwable, Database] =
    for
      bytes <- BinarySerialization.readBytes(path)
      db    <- deserialize(bytes)
    yield db
}

object BinarySerialization {

  def writeBytes(data: Array[Byte], path: String = "db.enc"): Either[Throwable, Unit] =
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
