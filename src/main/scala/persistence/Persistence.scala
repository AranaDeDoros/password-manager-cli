package org.aranadedoros
package persistence

import auth.Security.KeyProvider.given_SecretKey
import auth.Security.{Crypto, DecryptedPassword, EncryptedPassword, User}

import org.aranadedoros.model.Model.Database
import org.aranadedoros.serialization.Serialization.writeBytes

import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.collection.immutable.HashMap

object Persistence {
  object FileUtils:
    def init(user: String, password: String): Either[Throwable, Database] =
      val usr          = User(user, password)
      val db           = Database()
      val usrEncrypted = Crypto.encrypt(s"$usr".getBytes("UTF-8"))
      val contentEnc   = Crypto.encrypt("^".getBytes("UTF-8"))
      for
        _ <- writeBytes(data = usrEncrypted)
        _ <- writeBytes(data = contentEnc)
      yield db
}
