package net.liftweb.util
import _root_.org.apache.commons.codec.binary.Base64
import _root_.java.io.{InputStream, ByteArrayOutputStream, ByteArrayInputStream, Reader, File, FileInputStream, BufferedReader, InputStreamReader}
import _root_.java.security.{SecureRandom, MessageDigest}
import _root_.javax.crypto._
import _root_.javax.crypto.spec._

/**
 * The SecurityHelpers trait provides functions to:<ul>
 * <li> generate random numbers
 * <li> generate keys
 * <li> encrypt/decrypt keys
 * <li> create SHA, SHA-256, MD5 hashs (can be hex encoded)
 * </ul>
 */
trait SecurityHelpers { self: StringHelpers with IoHelpers =>

  /** short alias for java.security.SecureRandom */
  private val random = new java.security.SecureRandom

  /** return a random Long modulo a number */
  def randomLong(mod: Long): Long = Math.abs(random.nextLong) % mod

  /** return a random int modulo a number */
  def randomInt(mod: Int): Int = Math.abs(random.nextInt) % mod

  /**
   * return true only 'percent' times when asked repeatedly.
   * This function is used in the Skittr example to get a random set of users
   * @param percent percentage as a double number <= 1.0
   */
  def shouldShow(percent: Double): Boolean = random.nextDouble <= percent

  /** create a Blowfish key as an array of bytes */
  def makeBlowfishKey: Array[Byte] = KeyGenerator.getInstance("blowfish").generateKey.getEncoded

  /** create a Blowfish key from an array of bytes*/
  def blowfishKeyFromBytes(key: Array[Byte]): SecretKey = new SecretKeySpec(key, "blowfish")

  /** decrypt a Byte array with a Blowfish key (as a Byte array)*/
  def blowfishDecrypt(enc: Array[Byte], key: Array[Byte]): Array[Byte] = blowfishDecrypt(enc, blowfishKeyFromBytes(key))

  /** decrypt a Byte array with a Blowfish key (as a SecretKey object)*/
  def blowfishDecrypt(enc: Array[Byte], key: SecretKey): Array[Byte] = readWholeStream(decryptStream(new ByteArrayInputStream(enc), key))

  /** decrypt a Byte array with a Blowfish key (as a SecretKey object)*/
  def blowfishDecrypt(enc: String, key: Array[Byte]): String = blowfishDecrypt(enc, blowfishKeyFromBytes(key))

  /** decrypt a Byte array with a Blowfish key (as a SecretKey object)*/
  def blowfishDecrypt(enc: String, key: SecretKey): String = new String(blowfishDecrypt(base64Decode(enc), key), "UTF-8")

  /** encrypt a Byte array with a Blowfish key (as a Byte array)*/
  def blowfishEncrypt(plain: Array[Byte], key: Array[Byte]): Array[Byte] = blowfishEncrypt(plain, blowfishKeyFromBytes(key))

  /** encrypt a Byte array with a Blowfish key (as a SecretKey object)*/
  def blowfishEncrypt(plain: Array[Byte], key: SecretKey): Array[Byte] = readWholeStream(encryptStream(new ByteArrayInputStream(plain), key))

  /** encrypt a String with a Blowfish key (as a Byte array)*/
  def blowfishEncrypt(plain: String, key: Array[Byte]): String = blowfishEncrypt(plain, blowfishKeyFromBytes(key))

  /** encrypt a String with a Blowfish key (as a SecretKey object)*/
  def blowfishEncrypt(plain: String, key: SecretKey): String = base64Encode(blowfishEncrypt(plain.getBytes("UTF-8"), key))

  /** decrypt an InputStream with a Blowfish key (as a Byte array)*/
  def decryptStream(in: InputStream, key: Array[Byte]): InputStream = decryptStream(in, blowfishKeyFromBytes(key))

  /** decrypt an InputStream with a Blowfish key (as a SecretKey object)*/
  def decryptStream(in: InputStream, key: SecretKey): InputStream = {
    val cipher = Cipher.getInstance("blowfish")
    cipher.init(Cipher.DECRYPT_MODE, key)
    new CipherInputStream(in, cipher)
  }

  /** encrypt an InputStream with a Blowfish key (as a Byte array)*/
  def encryptStream(in: InputStream, key: Array[Byte]): InputStream= encryptStream(in, blowfishKeyFromBytes(key))

  /** encrypt an InputStream with a Blowfish key (as a SecretKey object)*/
  def encryptStream(in: InputStream, key: SecretKey): InputStream = {
    val cipher = Cipher.getInstance("blowfish")
    cipher.init(Cipher.ENCRYPT_MODE, key)
    new CipherInputStream(in, cipher)
  }

  /** encode a Byte array in Base 64 */
  def base64Encode(in: Array[Byte]): String = new String((new Base64).encode(in))

  /** decode a String in Base 64 */
  def base64Decode(in: String): Array[Byte] = (new Base64).decode(in.getBytes("UTF-8"))

  /** create a MD5 digest from a Byte array */
  def md5(in: Array[Byte]): Array[Byte] = (MessageDigest.getInstance("MD5")).digest(in)

  /** create a MD5 digest from a String */
  def md5(in: String): String = new String((new Base64) encode md5(in.getBytes("UTF-8")))

  /** create a SHA hash from a Byte array */
  def hash(in : Array[Byte]) : Array[byte] = {
    (MessageDigest.getInstance("SHA")).digest(in)
  }

  /** create a SHA hash from a String */
  def hash(in: String) : String = {
    new String((new Base64) encode (MessageDigest.getInstance("SHA")).digest(in.getBytes("UTF-8")))
  }

  /** create a SHA-256 hash from a Byte array */
  def hash256(in : Array[Byte]) : Array[byte] = {
    (MessageDigest.getInstance("SHA-256")).digest(in)
  }

  /** create a SHA-256 hash from a String */
  def hash256(in : String): String = {
    new String((new Base64) encode (MessageDigest.getInstance("SHA-256")).digest(in.getBytes("UTF-8")))
  }

  /** create an hex encoded SHA hash from a Byte array */
  def hexDigest(in: Array[Byte]): String = {
    val binHash = (MessageDigest.getInstance("SHA")).digest(in)
    hexEncode(binHash)
  }

  /** create an hex encoded SHA-256 hash from a Byte array */
  def hexDigest256(in: Array[Byte]): String = {
    val binHash = (MessageDigest.getInstance("SHA-256")).digest(in)
    hexEncode(binHash)
  }

  /** encode a Byte array as hexadecimal characters */
  def hexEncode(in: Array[Byte]): String = {
    val sb = new StringBuilder
    val len = in.length
    def addDigit(in: Array[byte], pos: int, len: int, sb: StringBuilder) {
      if (pos < len) {
        val b: int = in(pos)
        val msb = (b & 0xf0) >> 4
        val lsb = (b & 0x0f)
        sb.append((if (msb < 10) ('0' + msb).asInstanceOf[char] else ('a' + (msb - 10)).asInstanceOf[char]))
        sb.append((if (lsb < 10) ('0' + lsb).asInstanceOf[char] else ('a' + (lsb - 10)).asInstanceOf[char]))

        addDigit(in, pos + 1, len, sb)
      }
    }
    addDigit(in, 0, len, sb)
    sb.toString
  }

}
