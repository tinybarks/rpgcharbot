package com.github.tinybarks.rpgcharbot

import fs2._
import scala.language.higherKinds

object Pipes {
  /**
    * Stateful pipe that only lets through [[Mention]]s with IDs higher than the mentions that came before them.
    * This effectively filters out mentions that have been seen before.
    */
  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  def uniqueMentions[F[_]]: Pipe[F, Seq[Mention], Seq[Mention]] =
    in => in.scanSegments(0L) { (lastSeenId, seg) =>
      val maxId = seg.map(m => (0L +: m.map(_.id)).max).toList.head
      val realLastSeenId = if (lastSeenId == 0) maxId else lastSeenId
      seg.map(_.filter(_.id > realLastSeenId)).asResult(maxId)
    }

  /**
    * Pipe that turns a `Stream[F, Seq[A]]` into a `Stream[F, A]`.
    */
  def toA[F[_], A]: Pipe[F, Seq[A], A] =
    in => in.flatMap(seq => Stream.chunk(Chunk.seq(seq)))

  /**
    * Runs `f(x)` on each `x` in the stream but doesn't manipulate the stream elements.
    */
  def inspect[F[_], A](f: A => Unit): Pipe[F, A, A] =
    in => in.map { x => f(x); x }
}
