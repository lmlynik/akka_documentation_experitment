/*
rule = "scala:fix.Akkadocs_v1"
*/
import java.io.File
import java.util.UUID

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.ws.TextMessage
import akka.http.scaladsl.server.Directives
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.{Done, NotUsed}

import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.concurrent.Future

class openapi extends annotation.StaticAnnotation

object PersonRepo extends DefaultJsonProtocol with SprayJsonSupport {
  import app.domain.{Person, Pet}
  def person: Person = Person(UUID.randomUUID().timestamp(), "john", 46, Nil, None)

  implicit lazy val petFormat: RootJsonFormat[Pet] = jsonFormat4(Pet.apply)
  implicit lazy val personFormat: RootJsonFormat[Person] = jsonFormat5(Person.apply)
}

object App extends App with Directives with SprayJsonSupport {

  import PersonRepo._

  def connectSocket(deviceId: UUID, channelGroupName: Option[String]): Flow[Any, TextMessage.Strict, NotUsed] = {
    val matchIdSource = Source.actorRef[UUID](100, akka.stream.OverflowStrategy.fail)

    Flow.fromSinkAndSource(Sink.ignore, matchIdSource).map(m => TextMessage.Strict(m.toString))
  }

  def processFile(file: File): Future[Done] = Future.successful(Done)

  @openapi
  val routes = pathPrefix("admin") {
    path("user") {
      get {
        complete(person)
      } ~ post {
        entity(as[app.domain.Person]) {
          _ =>
            complete(StatusCodes.Created)
        }
      }
    } ~ path("ws" / JavaUUID) { deviceId =>
      parameters('channelGroupName.?) { channelGroupName =>
        get {
          handleWebSocketMessagesForOptionalProtocol(connectSocket(deviceId, channelGroupName), None)
        }
      }
    }
  } ~ pathPrefix("util") {
    uploadedFile("cert") {
      case (metadata, file) =>
        onSuccess(processFile(file)) {
          _ => complete(StatusCodes.OK)
        }
    }
  }
}
