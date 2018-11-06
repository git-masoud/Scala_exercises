import scala.collection.mutable.ArrayBuffer

/**
  * Represents a Tetris board -- essentially a 2-d grid
  * of booleans. Supports tetris pieces and row clearning.
  * Has an "undo" feature that allows clients to add and remove pieces efficiently.
  * Does not do any drawing or have any idea of pixels. Intead,
  * just represents the abtsract 2-d board.
  * See Tetris-Architecture.html for an overview.
  * *
  * This is the starter file version -- a few simple things are filled in already
  * *
  *
  * @author Nick Parlante
  * @version 1.0, Mar 1, 2001
  */
object Board {
  val PLACE_OK = 0
  val PLACE_ROW_FILLED = 1
  val PLACE_OUT_BOUNDS = 2
  val PLACE_BAD = 3
}

/**
  * Creates an empty board of the given width and height
  * measured in blocks.
  */
final class Board(val width: Int, val height: Int) {
  private var grid = Array.ofDim[Boolean](width, height)
  private var lastGrid = Array.ofDim[Boolean](width, height)

  private val DEBUG = true
  private var committed = true

  /**
    * Returns the max column height present in the board.
    * For an empty board this is 0.
    */
  def getMaxHeight: Int = {
        var highestY = 0
        for (x <- 0 to this.width - 1; y <- 0 to this.height - 1)
          if (lastGrid(x)(y) && y > highestY) highestY = y
        highestY
//    this.height
  }

  /**
    * Given a piece and an x, returns the y
    * value where the piece would come to rest
    * if it were dropped straight down at that x.
    * *
    * <p>
    * Implementation: use the skirt and the col heights
    * to compute this fast -- O(skirt length).
    */
  def dropHeight(piece: Piece, x: Int): Int = {
    var result = 0
    for (i <- 0 to piece.width - 1)
      result = Math.max(result, getColumnHeight(x + i) - piece.skirt(i))
    return result
  }

  /**
    * Returns the height of the given column --
    *i.e. the y value of the highest block + 1.
    * The height is 0 if the column contains no blocks.
    */
  def getColumnHeight(x: Int): Int = {
    var maxy = -1
    for (i <- 0 to this.height - 1) {
      if ((grid(x)(i) == true) && (i > maxy))
        maxy = i
    }
    maxy + 1
  }

  /**
    * Returns the number of filled blocks in
    * the given row.
    */
  def getRowWidth(y: Int): Int = {
    var w = 0
    for (i <- 0 to width - 1)
      if (grid(i)(y)) w += 1
    w
  }

  /**
    * Returns true if the given block is filled in the board.
    * Blocks outside of the valid width/height area
    * always return true.
    */
  final def getGrid(x: Int, y: Int): Boolean = if (x < 0 || x >= width || y < 0 || y >= height) return true
  else return grid(x)(y)

  /**
    * Attempts to add the body of a piece to the board.
    * Copies the piece blocks into the board grid.
    * Returns PLACE_OK for a regular placement, or PLACE_ROW_FILLED
    * for a regular placement that causes at least one row to be filled.
    * *
    * <p>Error cases:
    * If part of the piece would fall out of bounds, the placement
    * does not change the board at all, and PLACE_OUT_BOUNDS is returned.
    * If the placement is "bad" --interfering with existing blocks in the grid --
    * then the placement is halted partially complete and PLACE_BAD is returned.
    * An undo() will remove the bad placement.
    */
  def place(piece: Piece, x: Int, y: Int): Int = {
    //    if (!committed) throw new RuntimeException("commit error")
    //    committed = false
    //    if ((x < 0 || y < 0 || x + piece.width > this.width || y + piece.height > this.height))
    //      return Board.PLACE_OUT_BOUNDS
    //    var result = Board.PLACE_OK
    //    for (point <- piece.body) {
    //      if (grid(x + point.x)(y + point.y))
    //        return Board.PLACE_BAD
    //      grid(x + point.x)(y + point.y) = true
    //      if (getRowWidth(y + point.y) == this.width)
    //        result = Board.PLACE_ROW_FILLED
    //    }
    //    result
    if ((x < 0) || (y < 0) || ((x + piece.width) > this.width) || ((y + piece.height) > this.height))
      return Board.PLACE_OUT_BOUNDS
    for (point <- piece.body) {
      //      if (this.width >= x + piece.width)
      //        return Board.PLACE_OUT_BOUNDS
      if (grid(point.x + x)(point.y + y))
        return Board.PLACE_BAD
      grid(point.x + x)(point.y + y) = true
    }
    for (x <- 0 to this.height - 1) if (this.getRowWidth(x) == this.width)
      return Board.PLACE_ROW_FILLED
    return Board.PLACE_OK
  }

  private def copyGrid(source: Array[Array[Boolean]], target: Array[Array[Boolean]]) = {
    for (x <- 0 to source.length - 1; y <- 0 to source(0).length - 1)
      target(x)(y) = source(x)(y)
  }

  /**
    * Deletes rows that are filled all the way across, moving
    * things above down. Returns true if any row clearing happened.
    * *
    * <p>Implementation: This is complicated.
    * Ideally, you want to copy each row down
    * to its correct location in one pass.
    * Note that more than one row may be filled.
    */
  def clearRows: Boolean = {
    var result=false
    for (x <- 0 to this.height - 1)
      if (this.getRowWidth(x) == this.width)
        {
          swapRows(x + 1, x)
          clearRows
          result=true
        }
    result
  }

  def swapRows(sourceRowIndex: Int, destRowIndex: Int): Boolean = {
    //
    if (sourceRowIndex >= this.height) {
      for (i <- 0 to width - 1)
        grid(i)(destRowIndex) = false
      return false
    }
    else {
      for (i <- 0 to width - 1)
        grid(i)(destRowIndex) = grid(i)(sourceRowIndex)
      return swapRows(sourceRowIndex + 1, destRowIndex + 1)
    }


  }

  /**
    * If a place() happens, optionally followed by a clearRows(),
    * a subsequent undo() reverts the board to its state before
    * the place(). If the conditions for undo() are not met, such as
    * calling undo() twice in a row, then the second undo() does nothing.
    * See the overview docs.
    */
  def undo(): Unit = {
    copyGrid(lastGrid, grid)
  }

  /**
    * Puts the board in the committed state.
    * See the overview docs.
    */
  def commit(): Unit = {
    copyGrid(grid, lastGrid)
  }
}

