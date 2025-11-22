package org.aranadedoros

import inout.CommandHandler
import model.Model.Flag
import parsing.Parsers.FlagParser

import org.aranadedoros.auth.Security.{MasterHashStorage, PasswordManager}
import org.aranadedoros.inout.IO.{checkDB, enteredPassword}

object Main {

  def main(args: Array[String]): Unit =

    val key = sys.env.getOrElse("APP_SECRET", throw new Exception("APP_SECRET not set"))
    val raw = args.toList

    val flag =
      for
        valid <- checkDB()
        resultFlag <- if valid then
          FlagParser.parse(raw)
        else
          FlagParser.parse("--help" :: Nil)
      yield resultFlag

    flag match
      case Right(flag) => CommandHandler.handle(flag)
      case Left(error) =>
        CommandHandler.handle(Flag.Help)
}
