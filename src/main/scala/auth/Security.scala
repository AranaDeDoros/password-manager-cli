package org.aranadedoros
package auth

import auth.Security.KeyProvider.given_SecretKey
import de.mkammerer.argon2.{Argon2, Argon2Factory}
import java.nio.file.{Files, Path, Paths}

object Security {

  case class User(name: String, passwd: String):
    override def toString: String = s"$name | $passwd"

  opaque type SecretKey = Array[Byte]

  private object SecretKey:
    def fromEnv(env: String): SecretKey =
      val base64 = sys.env.getOrElse(env, throw new Exception(s"$env not set"))
      java.util.Base64.getDecoder.decode(base64)

  object KeyProvider:
    given SecretKey =
      SecretKey.fromEnv("APP_SECRET")

  import java.security.SecureRandom
  import javax.crypto.Cipher
  import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}

  object Crypto:

    def encrypt(dp: DecryptedPassword): EncryptedPassword =
      EncryptedPassword(
        encrypt(dp.value.getBytes(java.nio.charset.Charset.forName("UTF-8")))
      )

    def decrypt(ep: EncryptedPassword): DecryptedPassword =
      DecryptedPassword(
        new String(decrypt(ep.bytes), "UTF-8")
      )

    private val AES_KEY_SIZE   = 32  // 256 bits
    private val GCM_NONCE_SIZE = 12
    private val GCM_TAG_SIZE   = 128 // bits

    def encrypt(plain: Array[Byte])(using key: SecretKey): Array[Byte] =
      val rnd   = SecureRandom()
      val nonce = new Array[Byte](GCM_NONCE_SIZE)
      rnd.nextBytes(nonce)

      val cipher     = makeCipher(nonce, Cipher.ENCRYPT_MODE)
      val ciphertext = cipher.doFinal(plain)
      nonce ++ ciphertext

    def decrypt(data: Array[Byte])(using key: SecretKey): Array[Byte] =
      val (nonce, ciphertext) = data.splitAt(GCM_NONCE_SIZE)
      val cipher              = makeCipher(nonce, Cipher.DECRYPT_MODE)
      cipher.doFinal(ciphertext)

    private def makeCipher(nonce: Array[Byte], mode: Int)(using key: SecretKey): Cipher =
      val cipher: Cipher = Cipher.getInstance("AES/GCM/NoPadding")
      val spec           = new GCMParameterSpec(GCM_TAG_SIZE, nonce)
      val secretKey      = new SecretKeySpec(key, "AES")
      cipher.init(mode, secretKey, spec)
      cipher

  final case class DecryptedPassword(value: String):
    override def toString: String = "<DecryptedPassword>"

  final case class EncryptedPassword(bytes: Array[Byte]):
    override def toString: String = "<EncryptedPassword>"

  @deprecated("Reserved for future use. Use Crypto object directly.")
  trait PasswordEncryptor:
    def encrypt(dp: DecryptedPassword): EncryptedPassword
    def decrypt(ep: EncryptedPassword): DecryptedPassword

  class MasterPasswordService:

    private val argon2: Argon2 =
      Argon2Factory.create(Argon2Factory.Argon2Types.ARGON2id)

    def generateHash(password: String): String = {
      argon2.hash(
        3,
        1 << 15,
        1,
        password.toCharArray
      )
    }

    def verify(password: String, storedHash: String): Boolean =
      argon2.verify(storedHash, password.toCharArray)

  class PasswordManager(masterPasswordHash: String):
    private val master = new MasterPasswordService()

    private def authenticate(input: String): Boolean =
      master.verify(input, masterPasswordHash)

    def validate(master: String)(f: => Boolean): Either[String, Boolean] =
      if (authenticate(master)) Right(f)
      else Left("wrong master password")

  object MasterHashStorage:
    private val path = Paths.get("master.hash")

    def loadHash(): Either[Throwable, String] =
      try
        Right(Files.readString(path).trim)
      catch
        case e: Throwable =>
          Left(e)

    def saveHash(hash: String): Either[Throwable, Path] =
      try
        Right(Files.writeString(path, hash))
      catch
        case e: Throwable =>
          Left(e)

}
