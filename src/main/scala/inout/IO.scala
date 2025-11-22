package org.aranadedoros
package inout

import auth.Security.{EncryptedPassword, MasterHashStorage, MasterPasswordService, PasswordManager}

import IO.{checkDB, enteredPassword}
import model.Model.{Database, Entry, Flag}
import persistence.Persistence.FileUtils
import serialization.JsonSerialization

import org.aranadedoros.parsing.Parsers.FlagParser

import java.io.File
import scala.io.StdIn.readLine

object IO {
  def checkDB():  Either[Throwable, Boolean] =
    println("enter master password")
    val password: String = enteredPassword
    try
      val dbFile = new File("db.enc")
      if !dbFile.exists() then
        println("creating...")
        println("enter username")
        val usr = readLine()

        FileUtils.init(usr, password) match
          case Right(db) =>
            println(s"database created !")
            Right(true)
          case Left(err) =>
            println(s"error creating database: ${err.getStackTrace}")
            Left(err)
      else
        println("database already exists")
        val result =
          for
            hash <- MasterHashStorage.loadHash()
            mngr  = new PasswordManager(hash)
            validated <- mngr.validate(password) {
              password.trim.length > 0 && password.trim.length >= 6
            }.left.map(msg => new Exception(msg))
            finalResult <- if validated then
                Right(true)
            else
            Left(new Exception("Invalid master password")) // error
          yield finalResult
        result
    catch
        case e:
          NullPointerException => println(e.getMessage)
          Left(e)

  def enteredPassword: String =
    val console = System.console()
    if console == null then System.exit(1)
    val passwordChars = console.readPassword("Password: ")
    val password      = String(passwordChars)
    password
}

object CommandHandler:

  def handle(flag: Flag): Unit =
    flag match
      case Flag.Add(site, key) =>
        println(s"adding entry: ${key.value}")
        println("enter password")
        val password  = enteredPassword
        val encrypted = EncryptedPassword(password.getBytes("UTF-8"))
        val entry     = Entry(site, key, encrypted)
        val db = JsonSerialization.readDatabase() match
          case Right(d) => d
          case Left(_)  => Database()

        val newDb = db + entry
        JsonSerialization.writeDatabase(newDb) match
          case Right(_)  => println(newDb)
          case Left(err) => println(s"error writing database: ${err.getMessage}")

      case Flag.Delete(_, key) =>
        println(s"deleting entry: ${key.value}")
        val db = JsonSerialization.readDatabase() match
          case Right(d) => d
          case Left(_)  => Database()

        val newDb = db - key
        JsonSerialization.writeDatabase(newDb) match
          case Right(_)  => println(newDb)
          case Left(err) => println(s"error writing database: ${err.getMessage}")

      case Flag.ListAll =>
        JsonSerialization.readDatabase() match
          case Right(db) => println(db)
          case Left(err) => println(s"error reading DB: ${err.getMessage}")

      case Flag.Search(key) =>
        val db = JsonSerialization.readDatabase() match
          case Right(d) => d
          case Left(_)  => Database()
        println(db / key)

      case Flag.Init =>
        checkDB()

      case Flag.Help =>
        println("use a valid option (--add, --del, --list, --search, --init)")
