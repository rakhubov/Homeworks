package old

import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.data.{NonEmptyList, Validated}
import cats.effect.{ExitCode, IO, IOApp, _}
import cats.implicits.toSemigroupKOps
import cats.syntax.all._
import doobie.implicits._
import doobie.implicits.legacy.localdate
import doobie.{Fragment, Fragments, Meta, Transactor}
import io.circe.generic.auto._
import org.http4s.Method.POST
import org.http4s.circe.CirceEntityCodec.{
  circeEntityDecoder,
  circeEntityEncoder
}
import cats.syntax.all._
import fs2.{Pipe, Pull, Stream}
import fs2.concurrent.{Queue, Topic}
import old.Author
import org.graalvm.compiler.hotspot.replacements.HotSpotReplacementsUtil.config
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.{Close, Ping, Text}
import org.http4s.{HttpRoutes, ParseFailure, QueryParamDecoder}

import java.time.{LocalDate, Year}
import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

//object test2 extends IOApp {
//
//  def onReceive(): Pipe[IO, WebSocketFrame, Unit] = { stream =>
//    stream
//      .parEvalMapUnordered(config.parallelMessageProcessingRate) {
//        case Text(envelope, _) =>
//          handleEnvelope(envelope)
//        case Ping(data) =>
//          multiplexer.pong(context, data)
//        case Close(_) =>
//          StructuredLogger[IO].info(s"Received close frame.")
//        case unexpected =>
//          StructuredLogger[IO].info(
//            s"client sent unexpected WebSocket Frame: $unexpected"
//          )
//      }
//      .handleErrorWith(e =>
//        StructuredLogger[IO]
//          .error(e)(
//            s"Critical unresolvable internal error: ${e.getMessage}. Closing connection abruptly."
//          )
//          .eval
//      )
//  }
//}
