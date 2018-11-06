import java.awt._
import java.awt.event.{WindowAdapter, WindowEvent}

import javax.swing._
class JPieceTest( piece:Piece, width:Int , height:Int ) extends JComponent{
  protected var root: Piece = null
  setPreferredSize(new Dimension(width, height))
  root = piece
  /**
    * Draws the rotations from left to right.
    * Each piece goes in its own little box.
    */
  val MAX_ROTATIONS = 4

  override def paintComponent(g: Graphics): Unit = {
    g.setColor(Color.red)
    var i=1
    var nextPiece:Piece=piece
    do
    {
      drawPiece(g,nextPiece,new Rectangle((i-1) * width / MAX_ROTATIONS,0, width / MAX_ROTATIONS,height))
      nextPiece=nextPiece.nextRotation
      i += 1
    }
    while(!nextPiece.equals(piece))

  }

  /**
    * Draw the piece inside the given rectangle.
    */
  private def drawPiece(g: Graphics, piece: Piece, r: Rectangle)= {
//    g.setColor(Color.red)
//    g.drawRect(r.x,r.y, r.width,r.height)

    for(point<-piece.body) {
      if(piece.skirt(point.x)==point.y)
        g.setColor(Color.yellow)
      else
        g.setColor(Color.black)
      g.fillRect((point.x*25)+r.x,(r.height-(point.y*25))-22, 22,22)
    }
    g.setColor(Color.red)
    g.setFont(new Font("Helvetica Neue", Font.PLAIN, 16))
    g.drawString("w: " + piece.width + " h: " +piece.height,r.x,r.height);
  }
}
object JPieceTest{
  def main(args: Array[String]): Unit = {
    val frame = new JFrame("Piece Tester")
    val container = frame.getContentPane.asInstanceOf[JComponent]

    // Put in a BoxLayout to make a vertical list
    container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS))

    val pieces = Piece.getPieces

    var i = 0
    while ( {
      i < pieces.length
    }) {
      val test = new JPieceTest(pieces(i), 400, 100)
      container.add(test)
        i += 1;
    }

    // Size the window and show it on screen
    frame.pack()
    frame.setVisible(true)

    // Quit on window close
    frame.addWindowListener(new WindowAdapter() {
      override def windowClosing(e: WindowEvent): Unit = {
        System.exit(0)
      }
    })
  }
}