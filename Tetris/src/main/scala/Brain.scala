// Brain.java -- the interface for Tetris brains

object Brain { // Move is used as a struct to store a single Move
// ("static" here means it does not have a pointer to an
// enclosing Brain object, it's just in the Brain namespace.)
class Move {
  var x = 0
  var y = 0
  var piece: Piece = null
  var score = .0 // lower scores are better

}

}

trait Brain {
  /**
    * Given a piece and a board, returns a move object that represents
    * the best play for that piece, or returns null if no play is possible.
    * The board should be in the committed state when this is called.
    * "limitHeight" is the bottom section of the board that where pieces must
    * come to rest -- typically 20.
    * If the passed in move is non-null, it is used to hold the result
    * (just to save the memory allocation).
    */
  def bestMove(board: Board, piece: Piece, limitHeight: Int, move: Brain.Move): Brain.Move
}

