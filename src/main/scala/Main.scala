package org.aranadedoros

import inout.CommandHandler
import inout.IO.{enteredPassword, promptFlag}
import persistence.Persistence.DatabaseManager

object Main {

  def main(args: Array[String]): Unit =

    val key = sys.env.getOrElse("APP_SECRET", throw new Exception("APP_SECRET not set"))
    println(key)
    val raw      = Array[String]("--list")
    val password = enteredPassword
    val dbResult = DatabaseManager.loadOrCreate(password)

    dbResult match
      case Left(err) =>
        println(s"Error: ${err.getMessage}")
      case Right(db) =>
        promptFlag(raw) match
          case Left(err) => println(err)
          case Right(flag) =>
            CommandHandler.execute(db, flag, enteredPassword) match
              case Left(err) => println(s"Error executing command: ${err.getMessage}")
              case Right(_)  => println("Done")
}
