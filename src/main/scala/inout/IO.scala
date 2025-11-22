package org.aranadedoros
package inout

import model.Model.{Database, Entry, Flag}
import parsing.Parsers
import serialization.JsonSerialization

object IO {
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
}

object CommandHandler:
  def execute(db: Database, flag: Flag, passwordPrompt: => String): Either[Throwable, Database] =
    flag match

      case Flag.Add(site, key) =>
        val password  = passwordPrompt
        val encrypted = auth.Security.EncryptedPassword(password.getBytes("UTF-8"))
        val entry     = Entry(site, key, encrypted)
        val newDb     = db + entry
        JsonSerialization.writeDatabase(newDb).map(_ => newDb)

      case Flag.Delete(_, key) =>
        val newDb = db - key
        JsonSerialization.writeDatabase(newDb).map(_ => newDb)

      case Flag.ListAll =>
        println(db)
        Right(db)

      case Flag.Search(key) =>
        println(db / key)
        Right(db)

      case Flag.Init =>
        Right(db) // checkDB / init handled in DatabaseManager

      case Flag.Help =>
        println("use a valid option (--add, --del, --list, --search, --init)")
        Right(db)
