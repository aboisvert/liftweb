package net.liftweb.ext_api.facebook;

import java.net.{HttpURLConnection, URL, URLEncoder}
import java.io.DataOutputStream

import scala.xml.{Node, XML}

object FacebookRestApi {
  def apiKey = System.getProperty("com.facebook.api_key")
  def secret = System.getProperty("com.facebook.secret")
  def apiKey_=(key: String) = System.setProperty("com.facebook.api_key", key)
  def secret_=(key: String) = System.setProperty("com.facebook.secret", key)  
}

object FacebookClient {
  import FacebookRestApi._
  
  val TARGET_API_VERSION = "1.0"
  val FB_SERVER = "api.facebook.com/restserver.php"
  val SERVER_ADDR = "http://" + FB_SERVER
  val HTTPS_SERVER_ADDR = "https://" + FB_SERVER

  val SERVER_URL = new URL(SERVER_ADDR)
  val HTTPS_SERVER_URL = new URL(HTTPS_SERVER_ADDR)

  val CrLf = "\r\n"
  val Pref = "--"
  
  def urlEncode(name: String): String = URLEncoder.encode(name, "UTF-8")
      
  def stripSig(in: String): String =  if (in != null && in.startsWith("fb_sig_")) in.substring(7) else in

  def convert(in: List[(String, Any)]): List[String] = in.map{case (name, value) => stripSig(name)+"="+value}

  def byteToHex(b: Byte): String = Integer.toHexString((b & 0xf0) >>> 4) + Integer.toHexString(b & 0x0f)
    
  def genSignature(allParams: List[(String, Any)], secret: String): String = {
    val md = java.security.MessageDigest.getInstance("MD5")
    val theStr = convert(allParams).sort(_ < _).mkString("") + secret

    md.digest((theStr).getBytes).map(byteToHex(_)).mkString("")
  }

  private[facebook] def call(params: List[(String, Any)]): Node = {
    val theParams = params.map{case (name, value) => urlEncode(name)+"="+urlEncode(value.toString)}.mkString("&")

    SERVER_URL.openConnection match {
      case conn: HttpURLConnection => {
        conn.setRequestMethod("POST")
        conn.setDoOutput(true)
        conn.connect
        conn.getOutputStream.write(theParams.getBytes())

        XML.load(conn.getInputStream())
      }
    }
  }
  
  private[facebook] def buildParams(methodName: String, params: Seq[(String, Any)]): List[(String, Any)] = {
    val allParams: List[(String, Any)] =
      ("method", methodName) ::
      ("api_key", apiKey) ::
      ("v",  TARGET_API_VERSION) ::
      params.toList

    val signature = genSignature(allParams, secret)

    val ret = "sig" -> signature :: allParams
    ret
  }
  
  def callMethod(meth: SessionlessFacebookMethod, params: (String, Any)* ): Node =
    call(buildParams(meth.name, params))
    
  def authGetSession(authToken: String) : Option[String] = {
    (callMethod(AuthGetSession, ("auth_token", authToken)) \\ "session_key").text match {
      case "" => None
      case sessionKey => Some(sessionKey)
    }
  }
}
  
class FacebookClient(val apiKey: String, val secret: String, val sessionKey: String) {
  import FacebookRestApi._
  import FacebookClient._
  
  def this(sessionKey: String) = this(FacebookRestApi.apiKey, FacebookRestApi.secret, sessionKey)
  
  def callMethod(meth: FacebookMethod, name: String, mimeType: String, file: Array[Byte], params: (String, Any)* ): Node = {
    val boundary = System.currentTimeMillis.toString
    SERVER_URL.openConnection match {
      case conn: HttpURLConnection => {
        conn.setDoInput(true)
        conn.setDoOutput(true)
        conn.setUseCaches(false)
        conn.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + boundary)
        conn.setRequestProperty("MIME-version", "1.0")

        val out = new DataOutputStream(conn.getOutputStream())

        buildParams(meth, params).foreach {
          case (name, value) =>
            out.writeBytes(Pref + boundary + CrLf)
            out.writeBytes("Content-disposition: form-data; name=\"" + name + "\"")
            out.writeBytes(CrLf + CrLf + value.toString + CrLf)
        }

        out.writeBytes(Pref + boundary + CrLf)
        out.writeBytes("Content-disposition: form-data; filename=\"" +
                       name + "\"" + CrLf)
        out.writeBytes("Content-Type: " + mimeType + CrLf + CrLf)

        out.write(file)

        out.writeBytes(CrLf + Pref + boundary + Pref + CrLf)

        out.flush()
        out.close()

        XML.load(conn.getInputStream)
      }
    }
  }
    
  def callMethod(meth: FacebookMethod, params: (String, Any)* ): Node =
    call(buildParams(meth, params))
  
  private def buildParams(meth: FacebookMethod, params: Seq[(String, Any)]): List[(String, Any)] = {
    val allParams: List[(String, Any)] =
      (if (meth.requiresSession)
        List("call_id" -> System.currentTimeMillis, "session_key" -> sessionKey)
      else
        Nil) :::
      params.toList
    
    FacebookClient.buildParams(meth.name, allParams)
  }
      
  def getInfo(users: Collection[Int], fields: Collection[FacebookField]): Node = {
    callMethod(GetInfo, ("uids", users.mkString(",")), ("fields", fields.map(_.name).mkString(",")))
  }
}
  
class FacebookMethod(val name: String, paramCnt: Int, attachment: Boolean) {
  def this(nm: String, cnt: Int) { this(nm, cnt, false) }
  def this(nm: String) { this(nm, 0) }

  def requiresSession: Boolean = true
}

class SessionlessFacebookMethod(override val name: String, paramCnt: Int) extends FacebookMethod(name, paramCnt, false) {
  def this(nm: String) = this(nm, 0)
  
  override def requiresSession = false
}

case object AuthCreateToken extends SessionlessFacebookMethod("facebook.auth.createToken")
case object AuthGetSession extends SessionlessFacebookMethod("facebook.auth.getSession", 1)
case object GetFriends extends FacebookMethod("facebook.friends.get")
case object FqlQuery extends FacebookMethod("facebook.fql.query", 1)
case object GetEvents extends FacebookMethod("facebook.events.get", 5)
case object GetEventsMembers extends FacebookMethod("facebook.events.getMembers", 1)
case object GetAppUsers extends FacebookMethod(" facebook.friends.getAppUsers")
case object GetRequests extends FacebookMethod("facebook.friends.getRequests")
case object AreFriends extends FacebookMethod("facebook.friends.areFriends", 2)
case object GetPhotos extends FacebookMethod("facebook.photos.get", 2)
case object GetAlbums extends FacebookMethod("facebook.photos.getAlbums", 1)
case object GetPhotoTags extends FacebookMethod(" facebook.photos.getTags", 1)
case object CreatePhotoAlbum extends FacebookMethod("facebook.photos.createAlbum", 3)
case object AddPhotoTag extends FacebookMethod("facebook.photos.addTag", 5)
case object UploadPhoto extends FacebookMethod("facebook.photos.upload", 3, true)
case object GetInfo extends FacebookMethod("facebook.users.getInfo", 2)
case object GetUser extends FacebookMethod("facebook.users.getLoggedInUser", 1)
case object GetNotifications extends FacebookMethod("facebook.notifications.get")
case object SendNotifications extends FacebookMethod("facebook.notifications.send", 5)
case object SendRequest extends FacebookMethod("facebook.notifications.sendRequest", 5)
case object GetGroups extends FacebookMethod("facebook.groups.get", 1)
case object GetGroupMembers extends FacebookMethod("facebook.groups.getMembers", 1)
case object SetFBML extends FacebookMethod("facebook.profile.setFBML", 2)
case object GetFBML extends FacebookMethod("facebook.profile.getFBML", 1)
case object RefreshImage extends FacebookMethod("facebook.fbml.refreshImgSrc", 1)
case object RefreshRefURL extends FacebookMethod("facebook.fbml.refreshRefUrl", 1)
case object PublishStory extends FacebookMethod("facebook.feed.publishStoryToUser", 11)
case object PublishAction extends FacebookMethod("facebook.feed.publishActionOfUser", 11)
  
class FacebookField(val name: String)

case object AboutMe extends FacebookField("about_me")
case object Activities extends FacebookField("activities")
case object Affiliations extends FacebookField("affiliations")
case object Birthday extends FacebookField("birthday")
case object Books extends FacebookField("books")
case object CurrentLocation extends FacebookField("current_location")
case object EducationHistory extends FacebookField("education_history")
case object FirstName extends FacebookField("first_name")
case object AddedApp extends FacebookField("has_added_app")
case object Hometown extends FacebookField("hometown_location")
case object Highschool extends FacebookField("hs_info")
case object Interests extends FacebookField("interests")
case object AppUser extends FacebookField("is_app_user")
case object LastName extends FacebookField("last_name")
case object MeetingFor extends FacebookField("meeting_for")
case object LookingFor extends FacebookField("meeting_for")
case object MeetingSex extends FacebookField("meeting_sex")
case object InterestedIn extends FacebookField("meeting_sex")
case object Movies extends FacebookField("movies")
case object Music extends FacebookField("music")
case object Name extends FacebookField("name")
case object NotesCount extends FacebookField("notes_count")
case object Pic extends FacebookField("pic")
case object BigPic extends FacebookField("pic_big")
case object SmallPic extends FacebookField("pic_small")
case object SquarePic extends FacebookField("pic_square")
case object PoliticalView extends FacebookField("political")
case object UpdateTime extends FacebookField("profile_update_time")
case object Quotes extends FacebookField("quotes")
case object Relationship extends FacebookField("relationship_status")
case object RelationshipStatus extends FacebookField("relationship_status")
case object Religion extends FacebookField("religion")
case object Sex extends FacebookField("sex")
case object SignificantOther extends FacebookField("significant_other_id")
case object Status extends FacebookField("status")
case object Timezone extends FacebookField("timezone")
case object TV extends FacebookField("tv")
case object WallCount extends FacebookField("wall_count")
case object WorkHistory extends FacebookField("work_history")
