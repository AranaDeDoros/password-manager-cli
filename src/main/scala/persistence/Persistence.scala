package org.aranadedoros
package persistence

import auth.Security.{MasterHashStorage, MasterPasswordService, PasswordManager}
import model.Model.Database
import serialization.{BinarySerialization, JsonSerialization}
import java.io.File
import scala.io.StdIn.readLine

object Persistence:
  object FileUtils:
    def init(user: String, password: String): Either[Throwable, Array[Byte]] =
      // write for the 1st time
      val service = new MasterPasswordService()
      val hash    = service.generateHash(password)
      val db      = Database()
      for
        _          <- MasterHashStorage.saveHash(hash)
        serialized <- JsonSerialization.serialize(db)
        _          <- BinarySerialization.writeBytes(serialized)
      yield serialized

  object DatabaseManager:
    def loadOrCreate(password: String): Either[Throwable, Database] =
      val dbFile = new File("db.enc")
      if !dbFile.exists() then
        println("Database does not exist. Creating...")
        println("Enter username:")
        val usr = readLine()
        FileUtils.init(usr, password).flatMap(_ => JsonSerialization.readDatabase())
      else
        for
          hash <- MasterHashStorage.loadHash()
          mngr = new PasswordManager(hash)
          valid <- mngr.validate(password) {
            password.trim.nonEmpty && password.trim.length >= 6
          }.left.map(msg => new Exception(msg))
          db <- if valid then JsonSerialization.readDatabase()
          else Left(new Exception("Invalid master password"))
        yield db
