package code.snippet

import scala.xml.NodeSeq
import net.liftweb.http.js.JsCmds.Script
import net.liftweb.http.{ S, ScopedLiftActor }
import net.liftweb.http.js.JsObj
import net.liftweb.json.{ JField, JObject }
import net.liftweb.json.JString
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.json.JInt
import com.sun.tools.hat.internal.model.JavaDouble
import net.liftweb.json.JDouble

case class Recommendation(artist: String, album: String, name: String)

object ActNice {
  def render: NodeSeq = {
    (for {
      sess <- S.session
    } yield {
      // get a server-side actor that when we send
      // a JSON serializable object, it will send it to the client
      // and call the named function with the parameter
      val clientProxy = sess.serverActorForClient("changeNode")
      val recommendationProxy = sess.serverActorForClient("setRecommendation")

      // Create a server-side Actor that will receive messaes when
      // a function on the client is called
      val serverActor = new ScopedLiftActor {
        override def lowPriority = {
          case JString(str) => clientProxy ! ("You said: " + str)
          case JObject(JField("fname", JString(fname)) :: JField("lname", JString(lname)) :: JField("age", JString(age)) :: _) =>
            val rec = if (age.toInt > 40)    
              Recommendation("The Beatles", "The White Album", fname + " " + lname)
            else
              Recommendation("Lady Gaga", "Born This Way", fname + " " + lname)

            recommendationProxy ! rec
        }
      }

      Script(
        JsRaw("var sendToServer = " + sess.clientActorFor(serverActor).toJsCmd).cmd
      )
    }) openOr NodeSeq.Empty
  }
}
