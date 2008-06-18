package net.liftweb.util
import org.apache.commons.codec.binary.Base64
import java.io.{InputStream, ByteArrayOutputStream, ByteArrayInputStream, Reader, File, FileInputStream, BufferedReader, InputStreamReader}
import java.security.{SecureRandom, MessageDigest}
import javax.crypto._
import javax.crypto.spec._

trait SecurityHelpers { self: StringHelpers =>
  private val random = new java.security.SecureRandom
  
  def randomLong(mod: Long): Long = Math.abs(random.nextLong) % mod
  def randomInt(mod: Int): Int = Math.abs(random.nextInt) % mod
  
  def shouldShow(percent: Int): Boolean = Math.abs(random.nextInt) % 100 < percent
  def shouldShow(percent: Double): Boolean = random.nextDouble <= percent
  
  def makeBlowfishKey: Array[Byte] = KeyGenerator.getInstance("blowfish").generateKey.getEncoded
  def blowfishKeyFromBytes(key: Array[Byte]): SecretKey = new SecretKeySpec(key, "blowfish")
  
  def blowfishDecrypt(enc: Array[Byte], key: Array[Byte]): Array[Byte] = blowfishDecrypt(enc, blowfishKeyFromBytes(key))
  def blowfishDecrypt(enc: Array[Byte], key: SecretKey): Array[Byte] = readWholeStream(decryptStream(new ByteArrayInputStream(enc), key))
  
  def blowfishEncrypt(plain: String, key: Array[Byte]): String = blowfishEncrypt(plain, blowfishKeyFromBytes(key))
  def blowfishEncrypt(plain: String, key: SecretKey): String = base64Encode(blowfishEncrypt(plain.getBytes("UTF-8"), key))
  
  def blowfishDecrypt(enc: String, key: Array[Byte]): String = blowfishDecrypt(enc, blowfishKeyFromBytes(key))
  def blowfishDecrypt(enc: String, key: SecretKey): String = new String(blowfishDecrypt(base64Decode(enc), key), "UTF-8")
  
  def blowfishEncrypt(plain: Array[Byte], key: Array[Byte]): Array[Byte] = blowfishEncrypt(plain, blowfishKeyFromBytes(key))
  def blowfishEncrypt(plain: Array[Byte], key: SecretKey): Array[Byte] = readWholeStream(encryptStream(new ByteArrayInputStream(plain), key))
  
  
  def decryptStream(in: InputStream, key: Array[Byte]): InputStream = decryptStream(in, blowfishKeyFromBytes(key))
  def decryptStream(in: InputStream, key: SecretKey): InputStream = {
    val cipher = Cipher.getInstance("blowfish")
    cipher.init(Cipher.DECRYPT_MODE, key)
    new CipherInputStream(in, cipher)
  }
  
  def encryptStream(in: InputStream, key: Array[Byte]): InputStream= encryptStream(in, blowfishKeyFromBytes(key))
  def encryptStream(in: InputStream, key: SecretKey): InputStream = {
    val cipher = Cipher.getInstance("blowfish")
    cipher.init(Cipher.ENCRYPT_MODE, key)
    new CipherInputStream(in, cipher)
  }
  
  def base64Encode(in: Array[Byte]): String = {
    new String((new Base64).encode(in))
  }
  
  def base64Decode(in: String): Array[Byte] = {
    (new Base64).decode(in.getBytes("UTF-8"))
  }

  def readWholeFile(file: File): Array[Byte] = readWholeStream(new FileInputStream(file))
  
  def readWholeStream(in: InputStream): Array[Byte] = {
    val bos = new ByteArrayOutputStream
    val ba = new Array[byte](4096)
    
    def readOnce {
      val len = in.read(ba)
      if (len > 0) bos.write(ba, 0, len)
      if (len >= 0) readOnce
    }
    
    readOnce
    
    bos.toByteArray
  }
  
  def md5(in: Array[Byte]): Array[Byte] = (MessageDigest.getInstance("MD5")).digest(in)
  
  def md5(in: String): String = new String((new Base64) encode md5(in.getBytes("UTF-8")))
  
  def hash(in: String) : String = {
    new String((new Base64) encode (MessageDigest.getInstance("SHA")).digest(in.getBytes("UTF-8")))
  }
  
  def hash(in : Array[Byte]) : Array[byte] = {
    (MessageDigest.getInstance("SHA")).digest(in)
  }
  
  def hash256(in : Array[Byte]) : Array[byte] = {
    (MessageDigest.getInstance("SHA-256")).digest(in)
  }
  
  def hexDigest(in: Array[Byte]): String = {
    val binHash = (MessageDigest.getInstance("SHA")).digest(in)
    hexEncode(binHash)
  }
  
  
  def hash256(in : String): String = {
    new String((new Base64) encode (MessageDigest.getInstance("SHA-256")).digest(in.getBytes("UTF-8")))
  }
  
  def hexDigest256(in: Array[Byte]): String = {
    val binHash = (MessageDigest.getInstance("SHA-256")).digest(in)
    hexEncode(binHash)
  }
  
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
