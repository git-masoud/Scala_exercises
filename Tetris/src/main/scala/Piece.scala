import java.awt._
import scala.collection.mutable.ArrayBuffer

/**
  * An immutable representation of a tetris piece in a particular rotation.
  * Each piece is defined by the blocks that make up its body.
  * See the Tetris-Architecture.html for an overview.
  * *
  * This is the starter file version -- a few simple things are filled in already
  *
  * @author Masoud
  * @version 1.0, Mar 1, 2001
  */
final class Piece private(val body: Array[Point]) {
  val width: Int = body.map(_.x).max + 1
  val height: Int = body.map(_.y).max + 1
  val skirt: Array[Int] = {
    val sk = new Array[Int](width)
    for (i <- 0 to width - 1) {
      val t = body.filter(_.x == i)
      if (t.length > 1) sk(i) = t.map(_.y).min else sk(i) = t(0).y
    }
    sk
  }
  private var next: Piece = _ // "next" rotation
  def nextRotation: Piece =  next

  def equals(other: Piece): Boolean = {
    for (p <- body)
      if (!other.body.contains(p)) return false
    return true
  }
}

object Piece {
  private var pieces: Array[Piece] = _ // singleton array of first rotations

  def getPiece(points:Array[Point]): Piece = {
    piecerRow(new Piece(points))
  }

  /**
    * Returns an array containing the first rotation of
    * each of the 7 standard tetris pieces.
    * The next (counterclockwise) rotation can be obtained
    * from each piece with the @link #nextRotation() message.
    * In this way, the client can iterate through all the rotations
    * until eventually getting back to the first rotation.
    */
  def getPieces(): Array[Piece] = {
    pieces = Array(piecerRow(new Piece(parsePoints("0 0	0 1	0 2	0 3"))), // 0
      piecerRow(new Piece(parsePoints("0 0	0 1	0 2	1 0"))), // 1
      piecerRow(new Piece(parsePoints("0 0	1 0	1 1	1 2"))), // 2
      piecerRow(new Piece(parsePoints("0 0	1 0	1 1	2 1"))), // 3
      piecerRow(new Piece(parsePoints("0 1	1 1	1 0	2 0"))), // 4
      piecerRow(new Piece(parsePoints("0 0	0 1	1 0	1 1"))), // 5
      piecerRow(new Piece(parsePoints("0 0	1 0	1 1	2 0"))) // 6
    )
    pieces
  }
  /**
    * Given a string of x,y pairs ("0 0	0 1	0 2	1 0"), parses
    * the points into a Point[] array.
    * (Provided code)
    */
  private def parsePoints(content: String): Array[Point] = {
    val points: ArrayBuffer[Point] = new ArrayBuffer[Point]()
    try {
      val values = content.split("""\s+""").map(Integer.parseInt(_))

      for (i <- 0 to values.length - 1 by 2)
        points += new Point(values(i), values(i + 1))
    }
    catch {
      case ex: NumberFormatException => throw new RuntimeException("Could not parse x,y string:" + content); // cheap way to do assert
      case e: Exception => throw e
    }
    points.toArray
  }

  def piecerRow(piece: Piece): Piece = {
    var thisPiece = piece
    while (true) {
      val nextPiece = rotatePiece(thisPiece)
      if (!nextPiece.equals(piece)) {
        thisPiece.next = nextPiece
        thisPiece = nextPiece
      }
      else {
        thisPiece.next = piece
        return piece
      }
    }
    piece
  }

  private def rotatePiece(piece: Piece): Piece = {
    val points = new Array[Point](piece.body.length)
    for (i <- 0 to piece.body.length - 1)
      points(i) = new Point(-piece.body(i).y, piece.body(i).x)
    val minX = points.map(_.x).min
    for (i <- 0 to piece.body.length - 1)
      points(i).x -= minX
    new Piece(points)
  }
}