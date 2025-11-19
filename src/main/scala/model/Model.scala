package org.aranadedoros
package model

import auth.Security.{Crypto, DecryptedPassword, EncryptedPassword}

import scala.collection.immutable.HashMap

object Model {

  final case class EntryKey(value: String) extends AnyVal

  object EntryKey:
    def apply(value: String): Either[String, EntryKey] =
      if value.trim.nonEmpty then Right(new EntryKey(value))
      else Left("Key cannot be empty")

  final case class Site(value: String) extends AnyVal

  object Site:
    def apply(value: String): Either[String, Site] =
      if value.trim.nonEmpty then Right(new Site(value))
      else Left("Site cannot be empty")

  case class Entry(site: Site, key: EntryKey, encrypted: EncryptedPassword):
    def password: DecryptedPassword =
      Crypto.decrypt(encrypted)

  type EntriesDictionary = HashMap[EntryKey, Entry]

  case class Database(
      name: String = "persistence",
      entries: EntriesDictionary = HashMap.empty
  ):

    def +(entry: Entry): Database =
      copy(entries = entries + (entry.key -> entry))

    def -(key: EntryKey): Database =
      copy(entries = entries - key)

    override def toString: String =
      entries.map { case (k, v) => s"$k | ${v.site} | ${v.key}" }.mkString("\n")

    def /(key: EntryKey): Either[String, Entry] =
      val optionEntry = entries.find((k, v) => v.key == key)
      optionEntry match {
        case Some((k, v)) => Right(v)
        case None         => Left("Not found")
      }

    def searchEntry(key: EntryKey): Either[String, Entry] =
      /(key)
}
