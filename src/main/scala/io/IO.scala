package org.aranadedoros
package io

import org.aranadedoros.persistence.Persistence.FileUtils

import java.io.File
import scala.io.StdIn.readLine

object IO {
  def checkDB(): Unit =
    try
      val dbFile = new File("db.enc")
      if !dbFile.exists() then
        println("creating...")
        println("enter username")
        val usr = readLine()
        println("enter master password")
        val password: String = enteredPassword
        println(s"entered password (hidden): $password")
        FileUtils.init(usr, password) match
          case Right(db) =>
            println(s"database '${db}' created !")
          case Left(err) =>
            println(s"error creating database: ${err.getMessage}")
      else
        println("database already exists")
    catch
      case e: NullPointerException => println(e.getMessage)

  def enteredPassword: String =
    val console = System.console()
    if console == null then System.exit(1)
    val passwordChars = console.readPassword("Password: ")
    val password      = String(passwordChars)
    password
}
