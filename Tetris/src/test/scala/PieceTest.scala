import java.awt.Point

import org.scalatest.{BeforeAndAfter, FunSuite}

class PieceTest extends FunSuite with BeforeAndAfter {
  var pieceT: Piece = _
  var pieceLDog: Piece = _
  var pieceStick: Piece = _
  before {
    pieceT =  Piece.getPiece(Array(new Point(0,0),new Point(1,0),new Point(1,1),new Point(2,0))) // the T
    pieceLDog = Piece.getPiece(Array(new Point(0,1),new Point(1,1),new Point(1,0),new Point(2,0))) // the left dog
    pieceStick= Piece.getPiece(Array(new Point(0,0),new Point(0,1),new Point(0,2),new Point(0,3))) // the left dog
  }
  test("width"){
    assert(pieceT.width==3)
    assert(pieceLDog.width==3)
    assert(pieceStick.width==1)
  }
  test("height"){
    assert(pieceT.height==2)
    assert(pieceLDog.height==2)
    assert(pieceStick.height==4)
  }
  test("skirt"){
    assert(pieceT.skirt.length==3)
    assert(pieceT.skirt(0)==0)
    assert(pieceT.skirt(1)==0)
    assert(pieceT.skirt(2)==0)

    assert(pieceLDog.skirt.length==3)
    assert(pieceLDog.skirt(0)==1)
    assert(pieceLDog.skirt(1)==0)
    assert(pieceLDog.skirt(2)==0)

    assert(pieceStick.skirt.length==1)
    assert(pieceStick.skirt(0)==0)
  }

  test("comparison"){
    assert(!pieceLDog.equals(pieceT))
    assert(!pieceStick.equals(pieceT))
    assert(!pieceLDog.equals(pieceStick))
    val piece1_1=Piece.getPiece(Array(new Point(1,0),new Point(1,1),new Point(0,0),new Point(2,0)))
    val piece1_2=Piece.getPiece(Array(new Point(1,0),new Point(1,1),new Point(0,0),new Point(2,0)))
    val piece1_3=Piece.getPiece(Array(new Point(1,0),new Point(2,0),new Point(0,0),new Point(1,1)))
    assert(pieceT.equals(piece1_1) && pieceT.equals(piece1_2) && pieceT.equals(piece1_3))
  }
}
