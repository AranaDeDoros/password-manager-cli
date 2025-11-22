package org.aranadedoros
package inout

import auth.Security.{Crypto, DecryptedPassword}
import concurrency.SecureClipboard
import model.Model.{Database, Entry, Flag}
import parsing.Parsers
import serialization.JsonSerialization
import scala.concurrent.duration.DurationInt

object IO:
  def enteredPassword: String =
    val console = System.console()
    if console != null then
      val passwordChars = console.readPassword("Password: ")
      String(passwordChars)
    else
      println("Console not available. Using StdIn.readLine as fallback:")
      scala.io.StdIn.readLine("Password: ")

  def promptFlag(args: Array[String]): Either[String, Flag] =
    Parsers.FlagParser.parse(args.toList)

object CommandHandler:
  def execute(db: Database, flag: Flag, passwordPrompt: => String): Either[Throwable, Database] =
    flag match

      case Flag.Add(site, key) =>
        val password  = passwordPrompt
        val encrypted = Crypto.encrypt(DecryptedPassword(password))
        val entry     = Entry(site, key, encrypted)
        val newDb     = db + entry
        println(s"adding entry ${key}")
        JsonSerialization.writeDatabase(newDb).map(_ => newDb)

      case Flag.Delete(_, key) =>
        val newDb = db - key
        JsonSerialization.writeDatabase(newDb).map(_ => newDb)

      case Flag.ListAll =>
        println(db)
        Right(db)

      case Flag.Search(key) =>
        val db = JsonSerialization.readDatabase() match
          case Right(d) => d
          case Left(_)  => Database()
        db / key match
          case Right(entry) =>
            SecureClipboard.copyTemporarily(entry.password.value, 10.seconds)
            println(s"Password for entry '${key.value}' copied to clipboard for 10 seconds")
            Right(db)
          case Left(err) =>
            println(s"Entry not found: $err")
            Right(Database())

      case Flag.Init =>
        Right(db) // checkDB / init handled in DatabaseManager

      case Flag.Help =>
        println("use a valid option (--add, --del, --list, --search, --init)")
        Right(db)
