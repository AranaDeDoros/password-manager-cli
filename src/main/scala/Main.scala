package org.aranadedoros

import db.FileUtils
import db.Persistence.{Database, Entry}

import java.io.File
import scala.io.StdIn.readLine

object Main {

  private def parseCommand(args: Array[String]): Unit =
    args.toList match
      case "--add" :: title :: Nil =>
        println(s"adding entry: $title")
        val gh = Entry("GitHub.com", "github", "password")
//        val f: Entry => Database =
//          entry => Database(entries = hm + (entry.title -> entry))
        val bytes = FileUtils.readBytes() // read
        val db    = Database()            // deserialize
        val added = db + gh
        FileUtils.writeBytes(data = added.toString.getBytes("UTF-8"))

      case "--del" :: title :: Nil =>
        println(s"deleting entry: $title")
        val gh    = Entry("GitHub.com", "github", "password")
        val bytes = FileUtils.readBytes() // read
        val db    = Database()            // deserialize
        val added = db - gh
        FileUtils.writeBytes(data = added.toString.getBytes("UTF-8"))

      case "--list" :: Nil =>
        println("listing entries...")
        val bytes = FileUtils.readBytes() // read
        val db    = Database()
        println(db)

      case "--search" :: title :: Nil =>
        println(s"searching for: $title")
        val bytes = FileUtils.readBytes() // read
        val db    = Database()            // deserialize
        db / "github"

      case "--init" :: Nil =>
        println("init flag received: forcing database creation")
        checkDB()
      case "--help" :: Nil | Nil | List(_, _*) =>
        println("use a valid option (--add, --del, --list, --search, --init")

  private def checkDB(): Unit = {
    try
      val dbFile = new File("db.enc")
      if !dbFile.exists() then
        println("creating...")
        println("enter username")
        val usr = readLine()
        println("enter master password")
        val console = System.console()
        if console == null then System.exit(1)
        val passwordChars = console.readPassword("Password: ")
        val password      = String(passwordChars)
        println(s"entered password (hidden): $password")
        FileUtils.init(usr, password) match
          case Right(db) =>
            println(s"database '${db.name}' created !")
          case Left(err) =>
            println(s"error creating database: ${err.getMessage}")
      else
        println("database already exists")
    catch
      case e: NullPointerException => println(e.getMessage)
  }

  def main(args: Array[String]): Unit =

    val key = sys.env.getOrElse("APP_SECRET", throw new Exception("APP_SECRET not set"))
    println(s"clave $key")

    parseCommand(Array[String]("--init"))

}
