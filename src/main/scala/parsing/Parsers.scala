package org.aranadedoros
package parsing

import model.Model.{EntryKey, Flag, Site}

object Parsers {
  object FlagParser:
    def parse(args: List[String]): Either[String, Flag] =
      args match
        case "--add" :: rawSite :: rawKey :: Nil =>
          for
            site <- Site.fromRaw(rawSite)
            key  <- EntryKey.fromRaw(rawKey)
          yield Flag.Add(site, key)
        case "--del" :: rawSite :: rawKey :: Nil =>
          for
            site <- Site.fromRaw(rawSite)
            key  <- EntryKey.fromRaw(rawKey)
          yield Flag.Delete(site, key)
        case "--search" :: rawKey :: Nil =>
          EntryKey.fromRaw(rawKey).map(Flag.Search.apply)
        case "--list" :: Nil =>
          Right(Flag.ListAll)
        case "--init" :: Nil =>
          Right(Flag.Init)
        case "--help" :: Nil | Nil =>
          Right(Flag.Help)
        case other =>
          Left(s"Unknown command: ${other.mkString(" ")}")

}
