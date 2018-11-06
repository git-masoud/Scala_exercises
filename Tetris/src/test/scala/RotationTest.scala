import java.awt.Point
import org.scalatest.{BeforeAndAfter, FunSuite}

class RotationTest extends FunSuite with BeforeAndAfter {
  var pieceLDog: Piece = _
  var pieceLDogNext: Piece = _
  var pieceLL: Piece = _
  var pieceLLNext: Piece = _
  var pieceLLNextNext: Piece = _
  var pieceLLNextNextNext: Piece = _
  before {
    pieceLDog = Piece.getPiece(Array(new Point(0, 1), new Point(1, 1), new Point(1, 0), new Point(2, 0))) // the left dog
    pieceLDogNext = Piece.getPiece(Array(new Point(0, 0), new Point(0, 1), new Point(1, 1), new Point(1, 2)))
    pieceLL = Piece.getPiece(Array(new Point(0, 0), new Point(1, 0), new Point(1, 1), new Point(1, 2)))
    pieceLLNext = Piece.getPiece(Array(new Point(0, 1), new Point(1, 1), new Point(2, 1), new Point(2, 0)))
    pieceLLNextNext = Piece.getPiece(Array(new Point(0, 0), new Point(0, 1), new Point(0, 2), new Point(1, 2)))
    pieceLLNextNextNext = Piece.getPiece(Array(new Point(0, 0), new Point(0, 1), new Point(1, 0), new Point(2, 0)))
  }

  test("rotation function") {
    val lDogPiece = Piece.piecerRow(pieceLDog)
    assert(!lDogPiece.equals(pieceLDogNext))
    assert(lDogPiece.nextRotation.equals(pieceLDogNext))
    assert(lDogPiece.nextRotation.nextRotation.equals(pieceLDog))
    val lLPiece = Piece.piecerRow(pieceLL)
    assert(!lLPiece.equals(pieceLLNext))
    assert(lLPiece.nextRotation.equals(pieceLLNext))
    assert(lLPiece.nextRotation.nextRotation.equals(pieceLLNextNext))
    assert(lLPiece.nextRotation.nextRotation.nextRotation.equals(pieceLLNextNextNext))
    assert(lLPiece.nextRotation.nextRotation.nextRotation.nextRotation.equals(pieceLL))
    val lLNextNextNextPiece = Piece.piecerRow(pieceLLNextNextNext)
    assert(lLNextNextNextPiece.nextRotation.equals(pieceLL))
    assert(lLNextNextNextPiece.nextRotation.nextRotation.equals(pieceLLNext))
    assert(lLNextNextNextPiece.nextRotation.nextRotation.nextRotation.equals(pieceLLNextNext))
    assert(lLNextNextNextPiece.nextRotation.nextRotation.nextRotation.nextRotation.equals(pieceLLNextNextNext))
    assert(lLNextNextNextPiece.nextRotation.nextRotation.nextRotation.nextRotation.nextRotation.equals(pieceLL))
  }

}
