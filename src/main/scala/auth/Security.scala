package org.aranadedoros
package auth

import auth.Security.KeyProvider.given_SecretKey

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

  object Crypto extends PasswordEncryptor:

    override def encrypt(dp: DecryptedPassword): EncryptedPassword =
      EncryptedPassword(
        encrypt(dp.value.getBytes(java.nio.charset.Charset.forName("UTF-8")))
      )

    override def decrypt(ep: EncryptedPassword): DecryptedPassword =
      DecryptedPassword(
        new String(decrypt(ep.bytes), "UTF_8")
      )

    private val AES_KEY_SIZE   = 32  // 256 bits
    private val GCM_NONCE_SIZE = 12
    private val GCM_TAG_SIZE   = 128 // bits

    def encrypt(plain: Array[Byte])(using key: SecretKey): Array[Byte] =
      val rnd   = SecureRandom()
      val nonce = new Array[Byte](GCM_NONCE_SIZE)
      rnd.nextBytes(nonce)

      val cipher    = Cipher.getInstance("AES/GCM/NoPadding")
      val spec      = new GCMParameterSpec(GCM_TAG_SIZE, nonce)
      val secretKey = new SecretKeySpec(key, "AES")

      cipher.init(Cipher.ENCRYPT_MODE, secretKey, spec)
      val ciphertext = cipher.doFinal(plain)

      nonce ++ ciphertext

    def decrypt(data: Array[Byte])(using key: SecretKey): Array[Byte] =
      val (nonce, ciphertext) = data.splitAt(GCM_NONCE_SIZE)

      val cipher    = Cipher.getInstance("AES/GCM/NoPadding")
      val spec      = new GCMParameterSpec(GCM_TAG_SIZE, nonce)
      val secretKey = new SecretKeySpec(key, "AES")

      cipher.init(Cipher.DECRYPT_MODE, secretKey, spec)
      cipher.doFinal(ciphertext)

  final case class DecryptedPassword(value: String):
    override def toString: String = "<DecryptedPassword>"

  final case class EncryptedPassword(bytes: Array[Byte]):
    override def toString: String = "<EncryptedPassword>"

  trait PasswordEncryptor:
    def encrypt(dp: DecryptedPassword): EncryptedPassword
    def decrypt(ep: EncryptedPassword): DecryptedPassword

}
