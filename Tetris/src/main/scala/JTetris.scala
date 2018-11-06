import java.awt._

import javax.swing._
import java.util._
import java.awt.event._

import javax.swing.event._

import scala.util.control.Breaks
// JTetris.java


/**
  * JTetris presents a tetris game in a window.
  * It handles the GUI and the animation.
  * The Piece and Board classes handle the
  * lower-level computations.
  * This code is provided in finished form for the students.
  * See Tetris-Architecture.html for an overview.
  * *
  *
  * @author Nick Parlante
  * @version 1.0, March 1, 2001
  */
/*
 Implementation notes:
 -The "currentPiece" points to a piece that is
 currently falling, or is null when there is no piece.
 -tick() moves the current piece
 -a timer object calls tick(DOWN) periodically
 -keystrokes call tick with LEFT, RIGHT, etc.
 -Board.undo() is used to remove the piece from its
 old position and then Board.place() is used to install
 the piece in its new position.
*/ object JTetris { // size of the board in blocks
  val WIDTH = 10
  val HEIGHT = 20
  // Extra blocks at the top for pieces to start.
  // If a piece is sticking up into this area
  // when it has landed -- game over!
  val TOP_SPACE = 4
  val ROTATE = 0
  val LEFT = 1
  val RIGHT = 2
  val DROP = 3
  val DOWN = 4

  /**
    * Creates a Window,
    * installs the JTetris or JBrainTetris,
    * checks the testMode state,
    * install the controls in the WEST.
    */
  def main(args: Array[String]): Unit = {
    val frame = new JFrame("Tetris 2000")
    val container = frame.getContentPane.asInstanceOf[JComponent]
    container.setLayout(new BorderLayout)
    // Set the metal look and feel
    try
      UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName)
    catch {
      case ignored: Exception =>

    }
    // Could create a JTetris or JBrainTetris here
    val pixels = 16
    val tetris = new JTetris(WIDTH * pixels + 2, (HEIGHT + TOP_SPACE) * pixels + 2)
    container.add(tetris, BorderLayout.CENTER)
    if (args.length != 0 && args(0) == "test") tetris.testMode = true
    val panel = tetris.createControlPanel
    // Add the quit button last so it's at the bottom
    panel.add(Box.createVerticalStrut(12))
    val quit = new JButton("Quit")
    panel.add(quit)
    quit.addActionListener(new ActionListener() {
      override def actionPerformed(e: ActionEvent): Unit = {
        System.exit(0)
      }
    })
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

class JTetris(width: Int, height: Int) extends JComponent {
  setPreferredSize(new Dimension(width, height))
  gameOn = false
  pieces = Piece.getPieces
  board = new Board(JTetris.WIDTH, JTetris.HEIGHT + JTetris.TOP_SPACE)
  /*
       Register key handlers that call
       tick with the appropriate constant.
       e.g. 'j' and '4'  call tick(LEFT)
      */
  // LEFT
  registerKeyboardAction(new ActionListener() {
    override def actionPerformed(e: ActionEvent): Unit = {
      tick(JTetris.LEFT)
    }
  }, "left", KeyStroke.getKeyStroke('4'), JComponent.WHEN_IN_FOCUSED_WINDOW)
  registerKeyboardAction(new ActionListener() {
    override def actionPerformed(e: ActionEvent): Unit = {
      tick(JTetris.LEFT)
    }
  }, "left", KeyStroke.getKeyStroke('j'), JComponent.WHEN_IN_FOCUSED_WINDOW)
  // RIGHT
  registerKeyboardAction(new ActionListener() {
    override def actionPerformed(e: ActionEvent): Unit = {
      tick(JTetris.RIGHT)
    }
  }, "right", KeyStroke.getKeyStroke('6'), JComponent.WHEN_IN_FOCUSED_WINDOW)
  registerKeyboardAction(new ActionListener() {
    override def actionPerformed(e: ActionEvent): Unit = {
      tick(JTetris.RIGHT)
    }
  }, "right", KeyStroke.getKeyStroke('l'), JComponent.WHEN_IN_FOCUSED_WINDOW)
  // ROTATE
  registerKeyboardAction(new ActionListener() {
    override def actionPerformed(e: ActionEvent): Unit = {
      tick(JTetris.ROTATE)
    }
  }, "rotate", KeyStroke.getKeyStroke('5'), JComponent.WHEN_IN_FOCUSED_WINDOW)
  registerKeyboardAction(new ActionListener() {
    override def actionPerformed(e: ActionEvent): Unit = {
      tick(JTetris.ROTATE)
    }
  }, "rotate", KeyStroke.getKeyStroke('k'), JComponent.WHEN_IN_FOCUSED_WINDOW)
  // DROP
  registerKeyboardAction(new ActionListener() {
    override def actionPerformed(e: ActionEvent): Unit = {
      tick(JTetris.DROP)
    }
  }, "drop", KeyStroke.getKeyStroke('0'), JComponent.WHEN_IN_FOCUSED_WINDOW)
  registerKeyboardAction(new ActionListener() {
    override def actionPerformed(e: ActionEvent): Unit = {
      tick(JTetris.DROP)
    }
  }, "drop", KeyStroke.getKeyStroke('n'), JComponent.WHEN_IN_FOCUSED_WINDOW)

  // When this is true, plays a fixed sequence of 100 pieces
  protected var testMode = false
  final val TEST_LIMIT = 100
  // Is drawing optimized
  protected var DRAW_OPTIMIZE = true
  // Board data structures
  protected var board: Board = new Board(JTetris.WIDTH, JTetris.HEIGHT + JTetris.TOP_SPACE)
  protected var pieces: Array[Piece] = Piece.getPieces
  // The current piece in play or null
  protected var currentPiece: Piece = null
  protected var currentX = 0
  protected var currentY = 0
  protected var moved = false // did the player move the piece

  // The piece we're thinking about playing
  // -- set by computeNewPosition
  protected var newPiece: Piece = null
  protected var newX = 0
  protected var newY = 0
  // State of the game
  protected var gameOn = false // true if we are playing

  protected var count = 0 // how many pieces played so far

  protected var startTime = 0L // used to measure elapsed time

  protected var random: Random = null // the random generator for new pieces

  // Controls
  protected var countLabel: JLabel = null
  protected var timeLabel: JLabel = null
  protected var startButton: JButton = null
  protected var stopButton: JButton = null
  protected var timer:javax.swing.Timer =  new javax.swing.Timer(DELAY, new ActionListener() {
    override def actionPerformed(e: ActionEvent): Unit = {
      tick(JTetris.DOWN)
    }
  })

  protected var speed: JSlider = null
  final val DELAY = 400 // milliseconds per tick

  /**
    * Sets the internal state and starts the timer
    * so the game is happening.
    */
  def startGame(): Unit = { // cheap way to reset the board state
    board = new Board(JTetris.WIDTH, JTetris.HEIGHT + JTetris.TOP_SPACE)
    // draw the new board state once
    repaint()
    count = 0
    gameOn = true
    if (testMode) random = new Random(0) // same seq every time
    else random = new Random // diff seq each game
    enableButtons()
    timeLabel.setText(" ")
    addNewPiece()
    timer.start()
    startTime = System.currentTimeMillis
  }

  /**
    * Sets the enabling of the start/stop buttons
    * based on the gameOn state.
    */
  private def enableButtons(): Unit = {
    startButton.setEnabled(!gameOn)
    stopButton.setEnabled(gameOn)
  }

  /**
    * Stops the game.
    */
  def stopGame(): Unit = {
    gameOn = false
    enableButtons()
    timer.stop()
    val delta = (System.currentTimeMillis - startTime) / 10
    timeLabel.setText((delta / 100.0).toString + " seconds")
  }

  /**
    * Given a piece, tries to install that piece
    * into the board and set it to be the current piece.
    * Does the necessary repaints.
    * If the placement is not possible, then the placement
    * is undone, and the board is not changed. The board
    * should be in the committed state when this is called.
    * Returns the same error code as Board.place().
    */
  def setCurrent(piece: Piece, x: Int, y: Int): Int = {
    val result = board.place(piece, x, y)
    if (result <= Board.PLACE_ROW_FILLED) { // SUCESS
      // repaint the rect where it used to be
      if (currentPiece != null) repaintPiece(currentPiece, currentX, currentY)
      currentPiece = piece
      currentX = x
      currentY = y
      // repaint the rect where it is now
      repaintPiece(currentPiece, currentX, currentY)
    }
    else board.undo()
    result
  }

  /**
    * Selects the next piece to use using the random generator
    * set in startGame().
    */
  def pickNextPiece: Piece = {
    var pieceNum = 0
    pieceNum = (pieces.length * random.nextDouble).toInt
    val piece = pieces(pieceNum)
    piece
  }

  /**
    * Tries to add a new random at the top of the board.
    * Ends the game if it's not possible.
    */
  def addNewPiece(): Unit = {
    count += 1
    if (testMode && count == TEST_LIMIT + 1) {
      stopGame()
      return
    }
    val piece = pickNextPiece
    // Center it up at the top
    val px = (board.width - piece.width) / 2
    val py = board.height - piece.height
    // commit things the way they are
    board.commit()
    currentPiece = null
    // add the new piece to be in play
    val result = setCurrent(piece, px, py)
    // This probably never happens, since
    // the blocks at the top allow space
    // for new pieces to at least be added.
    if (result > Board.PLACE_ROW_FILLED) stopGame()
    countLabel.setText(Integer.toString(count))
  }

  /**
    * Figures a new position for the current piece
    * based on the given verb (LEFT, RIGHT, ...).
    * The board should be in the committed state --
    *i.e. the piece should not be in the board at the moment.
    * This is necessary so dropHeight() may be called without
    * the piece "hitting itself" on the way down.
    * *
    * Sets the ivars newX, newY, and newPiece to hold
    * what it thinks the new piece position should be.
    * (Storing an intermediate result like that in
    * ivars is a little tacky.)
    */
  def computeNewPosition(verb: Int): Unit = { // As a starting point, the new position is the same as the old
    newPiece = currentPiece
    newX = currentX
    newY = currentY
    // Make changes based on the verb
    verb match {
      case JTetris.LEFT =>
        newX -= 1
      case JTetris.RIGHT =>
        newX += 1
      case JTetris.ROTATE =>
        newPiece = newPiece.nextRotation
        // tricky: make the piece appear to rotate about its center
        // can't just leave it at the same lower-left origin as the
        // previous piece.
        newX = newX + (currentPiece.width - newPiece.width) / 2
        newY = newY + (currentPiece.height - newPiece.height) / 2
      case JTetris.DOWN =>
        newY -= 1
      case JTetris.DROP =>
        // note: if the piece were in the board, it would interfere here
        newY = board.dropHeight(newPiece, newX)
      case _ =>
        throw new RuntimeException("Bad verb")
    }
  }

  /**
    * Called to change the position of the current piece.
    * Each key press call this once with the verbs
    * LEFT RIGHT ROTATE DROP for the user moves,
    * and the timer calls it with the verb DOWN to move
    * the piece down one square.
    * *
    * Before this is called, the piece is at some location in the board.
    * This advances the piece to be at its next location.
    * *
    * Overriden by the brain when it plays.
    */
  def tick(verb: Int): Unit = {
    if (!gameOn) return
    if (currentPiece != null) board.undo() // remove the piece from its old position
    // Sets the newXXX ivars
    computeNewPosition(verb)
    // try out the new position (rolls back if it doesn't work)
    val result = setCurrent(newPiece, newX, newY)
    // if row clearing is going to happen, draw the
    // whole board so the green row shows up
    if (result == Board.PLACE_ROW_FILLED) repaint()
    val failed = result >= Board.PLACE_OUT_BOUNDS
    // if it didn't work, put it back the way it was
    if (failed) if (currentPiece != null) board.place(currentPiece, currentX, currentY)
    /*
         How to detect when a piece has landed:
         if this move hits something on its DOWN verb,
         and the previous verb was also DOWN (i.e. the player was not
         still moving it),  then the previous position must be the correct
         "landed" position, so we're done with the falling of this piece.
        */ if (failed && verb == JTetris.DOWN && !moved) { // it's landed
      if (board.clearRows) repaint() // repaint to show the result of the row clearing
      // if the board is too tall, we've lost
      if (board.getMaxHeight > board.height - JTetris.TOP_SPACE)
        stopGame()
      else { // Otherwise add a new piece and keep playing
        addNewPiece()
      }
    }
    // Note if the player made a successful non-DOWN move --
    // used to detect if the piece has landed on the next tick()
    moved = !(failed) && verb != JTetris.DOWN
  }

  /**
    * Given a piece and a position for the piece, generates
    * a repaint for the rectangle that just encloses the piece.
    */
  def repaintPiece(piece: Piece, x: Int, y: Int): Unit = {
    if (DRAW_OPTIMIZE) {
      val px = xPixel(x)
      val py = yPixel(y + piece.height - 1)
      val pwidth = xPixel(x + piece.width) - px
      val pheight = yPixel(y - 1) - py
      repaint(px, py, pwidth, pheight)
    }
    else repaint()
  }

  // width in pixels of a block
  final private def dX = ((width - 2).toFloat) / board.width

  // height in pixels of a block
  final private def dY = ((height - 2).toFloat) / board.height

  // the x pixel coord of the left side of a block
  final private def xPixel(x: Int) = 1 + (x * dX).round

  // the y pixel coord of the top of a block
  final private def yPixel(y: Int) = height - 1 - (y + 1) * dY.round

  /**
    * Draws the current board with a 1 pixel border
    * around the whole thing. Uses the pixel helpers
    * above to map board coords to pixel coords.
    * Draws rows that are filled all the way across in green.
    */
  override def paintComponent(g: Graphics): Unit = { // Draw a rect around the whole thing
    g.drawRect(0, 0, width - 1, height - 1)
    // Draw the line separating the top
    val spacerY = yPixel(board.height - JTetris.TOP_SPACE - 1)
    g.drawLine(0, spacerY, width - 1, spacerY)
    // check if we are drawing with clipping
    //Shape shape = g.getClip();
    var clip:Rectangle = null
    if (DRAW_OPTIMIZE) clip = g.getClipBounds
    // Factor a few things out to help the optimizer
    val dx = math.round(this.dX - 2)
    val dy = math.round(this.dY - 2)
    val bWidth = board.width
    val bHeight = board.height
    var x = 0
    var y = 0
    // Loop through and draw all the blocks
    // left-right, bottom-top
    x = 0
    for ( x <- 0 to bWidth-1){
      val left = xPixel(x)
      // the left pixel
      // right pixel (useful for clip optimization)
      val right = xPixel(x + 1) - 1
      // skip this x if it is outside the clip rect
      if (DRAW_OPTIMIZE && clip != null) {
        Breaks.breakable {
          if ((right < clip.x) || (left >= (clip.x + clip.width))) Breaks.break //todo: continue is not supported}
          // draw from 0 up to the col height
          val yHeight = board.getColumnHeight(x)
          y = 0
          while ( {
            y < yHeight
          }) {
            if (board.getGrid(x, y)) {
              val filled = board.getRowWidth(y) == bWidth
              if (filled) g.setColor(Color.green)
              g.fillRect(left + 1, yPixel(y) + 1, dx, dy) // +1 to leave a white border

              if (filled) g.setColor(Color.black)
            }

            {
              y += 1;
              y - 1
            }
          }


        }
      }
    }
  }

    /**
      * Updates the timer to reflect the current setting of the
      * speed slider.
      */
    def updateTimer(): Unit = {
      val value = speed.getValue.toDouble / speed.getMaximum
      timer.setDelay((DELAY - value * DELAY).toInt)
    }

    /**
      * Creates the panel of UI controls.
      * This code is very repetitive -- the GUI/XML
      * extensions in Java 1.4 should make this sort
      * of ugly code less necessary.
      */
    def createControlPanel = {
      val panel = Box.createVerticalBox
      // COUNT
      countLabel = new JLabel("0")
      panel.add(countLabel)
      // TIME
      timeLabel = new JLabel(" ")
      panel.add(timeLabel)
      panel.add(Box.createVerticalStrut(12))
      // START button
      startButton = new JButton("Start")
      panel.add(startButton)
      startButton.addActionListener(new ActionListener() {
        override def actionPerformed(e: ActionEvent): Unit = {
          startGame()
        }
      })
      // STOP button
      stopButton = new JButton("Stop")
      panel.add(stopButton)
      stopButton.addActionListener(new ActionListener() {
        override def actionPerformed(e: ActionEvent): Unit = {
          stopGame()
        }
      })
      enableButtons()
      val row = new JPanel
      // SPEED slider
      panel.add(Box.createVerticalStrut(12))
      row.add(new JLabel("Speed:"))
      speed = new JSlider(0, 200, 75) // min, max, current

      speed.setPreferredSize(new Dimension(100, 15))
      if (testMode) speed.setValue(200) // max for test mode
      updateTimer()
      row.add(speed)
      panel.add(row)
      speed.addChangeListener(new ChangeListener() { // when the slider changes, sync the timer to its value
        override def stateChanged(e: ChangeEvent): Unit = {
          updateTimer()
        }
      })
      panel
    }
  }
	