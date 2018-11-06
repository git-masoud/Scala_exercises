import java.awt._
import javax.swing._
import java.awt.event._

/**
  * JBoardTest is a unit test application for tetris -- its
  * sole purpose is exercising the tetris classes.
  * Its interface only make sense if you think of it
  * as a direct connection to the Board and Piece interfaces.
  * JBoardTest is provided in finished form so the students
  * can use it to test their Piece and Board code.
  * *
  * <p>
  * Use the buttons on screen or the
  * keys 4, 5, 6, 7, 0 for control.
  * Add a piece, move it around.
  * JBoardTest uses place() and undo() to
  * move the piece around the board.
  * Use the app to set up and test out specific
  * cases for your tetris board and piece.
  * *
  * <p>
  * <b>Transparency</b> --
  * Board has all sorts of hidden state --
  * heights, widths etc.. The hidden state
  * is what makes Board hard to debug.
  * The unit test tries to make the state of the Board
  * transparent, so you can see what's going on as you exercise
  * your code.
  */
object JBoardTest {
  /*
    Creates a frame for the board tester
    and its controls.
   */ def main(args: Array[String]): Unit = {
    val frame = new JFrame("JBoardTest")
    val container = frame.getContentPane.asInstanceOf[JComponent]
    container.setLayout(new BorderLayout)
    val bt = new JBoardTest(300, 600)
    container.add(bt, BorderLayout.CENTER)
    val panel = bt.createControlPanel
    container.add(panel, BorderLayout.EAST)
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

class JBoardTest(width: Int, height: Int) extends JComponent {
  setPreferredSize(new Dimension(width, height))
  reset()
  // Board data structures
  private var board:Board = _
  private var pieces = Piece.getPieces
  private var label:JLabel = _
  // The currently played piece
  // null if there is no current piece
  private var piece:Piece = _
  private var pieceX = 0
  private var pieceY = 0

  private def reset(): Unit = {
    board = new Board(      10, 24)
    piece = null
  }

  /**
    * Whenever the user clicks a button, this runs
    * to make the change. The strategy is:
    * compute what the new piece position should be,
    * undo to remove the old piece position,
    * put in the new piece position.
    */
  final val ADD = 1
  final val ADDRANDOM = 2
  final val LEFT = 3
  final val RIGHT = 4
  final val ROTATE = 5
  final val DOWN = 6
  final val DROP = 7
  final val CLEAR = 8
  final val RESET = 9
  final val UNDO = 10

  def action(verb: Int): Unit = {
    if (verb == RESET) reset()
  else if (verb == UNDO) board.undo()
   else if (verb == CLEAR){
    if (board.clearRows) {
      board.commit() // finalize the last position
 piece = null
    }
}
 else { // compute a new piece based on the old piece and the verb
    var newPiece = piece
   var newX = pieceX
      var newY = pieceY
      verb match {
        case ADD | ADDRANDOM =>
          board.commit()
          piece = null
          if (verb == ADD) newPiece = pieces(0)
          else newPiece = pieces((Math.random * pieces.length).toInt)
          newX = 0
          newY = board.height - newPiece.height - 1
        case LEFT =>
          newX -= 1
        case RIGHT =>
          newX += 1
        case ROTATE =>
          if (piece != null) newPiece = piece.nextRotation
        case DOWN =>
          newY -= 1
        case DROP =>
          // remove the piece before the drop computation so
          // it doesn't hit itself on the way down!
          if (piece != null) {
            board.undo()
            newY = board.dropHeight(newPiece, newX)
          }
        case _ =>
          throw new RuntimeException("Bad verb")
      }
      // now we have a newPiece
      if (newPiece != null) {
        board.undo() // remove the old state
        // Try putting in the new piece
        val result = board.place(newPiece, newX, newY)
        // See if it worked
        if (result <= Board.PLACE_ROW_FILLED) {
          piece = newPiece
          pieceX = newX
          pieceY = newY
        }
        else { // System.out.println("bad placement:" + result);
          label.setText("bad:" + result)
          // put it back the way it was
          //
          board.undo()
          if (piece != null) board.place(piece, pieceX, pieceY)
        }
      }
    }
    // Redraw the whole board since we've changed it
    repaint()  }

  /**
    * Pixel helpers -- these centralize how
    * we map from block coords (x,y) to pixel
    * co-ords on screen. By centralizing here,
    * at least it's all consistent.
    * These return the distance in pixels
    * from the left or botton edge
    * to the upper left corner of the rect
    * for each tetris block.
    */
  private def xPixelDist(x: Int, dx: Float) = MARGIN + x * dx.round

  private def yPixelDist(y: Int, dy: Float) = (y + 1) * dy.round

  // pixel space on the left and bottom
  final val MARGIN = 20

  override def paintComponent(g: Graphics): Unit = { // area where tetris blocks are drawn
    val pixelWidth = getWidth - MARGIN
    val pixelHeight = getHeight - MARGIN
    // block size of the board
    val bWidth = board.width
    val bHeight = board.height
    val bMaxHeight = board.getMaxHeight
    // pixels across of each block
    val dx = pixelWidth.toFloat / bWidth
    val dy = pixelHeight.toFloat / bHeight
    g.drawRect(MARGIN, 0, pixelWidth - 1, pixelHeight - 1)
    var x = 0
    var y = 0
    // draw the board blocks and the bits of text...
    y = 0
    while ( {
      y < bMaxHeight
    }) { // draw row width on the left
      g.drawString(Integer.toString(board.getRowWidth(y)), 4, pixelHeight - yPixelDist(y - 1, dy) - 4)
      x = 0
      while ( {
        x < bWidth
      }) {
        if (board.getGrid(x, y)) { // emphasize the col height in green
          if (board.getColumnHeight(x) == y + 1) g.setColor(Color.green)
          // draw the block, inset by one pixel all around
          g.fillRect(xPixelDist(x, dx) + 1, pixelHeight - yPixelDist(y, dy) + 1, dx.toInt - 2, dy.toInt - 2)
          g.setColor(Color.black)
        }

        {
          x += 1; x - 1
        }
      }

      {
        y += 1; y - 1
      }
    }
    // draw the height numbers along the bottom
    x = 0
    while ( {
      x < bWidth
    }) {
      g.drawString(Integer.toString(board.getColumnHeight(x)), xPixelDist(x, dx) + 6, pixelHeight + MARGIN - 4)

      {
        x += 1; x - 1
      }
    }
  }

  /**
    * Creates all the buttons in a panel, and wire them
    * to the action() message.
    * *
    * Installs keyboard listeners and wire them to the action()
    * message as well.
    */
  def createControlPanel: Container = {
    val panel = new Box(BoxLayout.Y_AXIS)
    var button:JButton = new JButton("Add")
    panel.add(button)
    button.addActionListener(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        action(ADD)
      }
    })
    button = new JButton("Add Random")
    panel.add(button)
    button.addActionListener(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        action(ADDRANDOM)
      }
    })
    button = new JButton("Left")
    panel.add(button)
    button.addActionListener(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        action(LEFT)
      }
    })
    button = new JButton("Right")
    panel.add(button)
    button.addActionListener(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        action(RIGHT)
      }
    })
    button = new JButton("Rotate")
    panel.add(button)
    button.addActionListener(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        action(ROTATE)
      }
    })
    button = new JButton("Down")
    panel.add(button)
    button.addActionListener(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        action(DOWN)
      }
    })
    button = new JButton("Drop")
    panel.add(button)
    button.addActionListener(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        action(DROP)
      }
    })
    button = new JButton("Clear Rows")
    panel.add(button)
    button.addActionListener(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        action(CLEAR)
      }
    })
    label = new JLabel
    panel.add(label)
    panel.add(Box.createVerticalStrut(12))
    button = new JButton("Undo")
    panel.add(button)
    button.addActionListener(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        action(UNDO)
      }
    })
    button = new JButton("Reset")
    panel.add(button)
    button.addActionListener(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        action(RESET)
      }
    })
    button = new JButton("Quit")
    panel.add(button)
    button.addActionListener(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        System.exit(0)
      }
    })
    // Register the 4 keystroke listeners
    // to call action() with the appropriate constant.
    // 4=left 5=rotate 6=right 0=drop 7=add
    registerKeyboardAction(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        action(ADD)
      }
    }, "add", KeyStroke.getKeyStroke('7'), JComponent.WHEN_IN_FOCUSED_WINDOW)
    registerKeyboardAction(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        action(LEFT)
      }
    }, "left", KeyStroke.getKeyStroke('4'), JComponent.WHEN_IN_FOCUSED_WINDOW)
    registerKeyboardAction(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        action(RIGHT)
      }
    }, "right", KeyStroke.getKeyStroke('6'), JComponent.WHEN_IN_FOCUSED_WINDOW)
    registerKeyboardAction(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        action(ROTATE)
      }
    }, "rotate", KeyStroke.getKeyStroke('5'), JComponent.WHEN_IN_FOCUSED_WINDOW)
    registerKeyboardAction(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        action(DROP)
      }
    }, "drop", KeyStroke.getKeyStroke('0'), JComponent.WHEN_IN_FOCUSED_WINDOW)
    panel
  }
}
