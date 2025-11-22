package org.aranadedoros
package persistence

import auth.Security.{MasterHashStorage, MasterPasswordService}
import model.Model.Database
import serialization.JsonSerialization

object Persistence {
  object FileUtils:
    def init(user: String, password: String): Either[Throwable, Array[Byte]] =
      // write for the 1st time
      val service = new MasterPasswordService()
      val hash    = service.generateHash(password)
      val db      = Database()
      for
        _   <- MasterHashStorage.saveHash(hash)
        ndb <- JsonSerialization.serialize(db)
        serialized = ndb
      yield serialized
}
