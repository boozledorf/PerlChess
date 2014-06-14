PerlChess
=========

Play a nice game of chess!

==6/13/2014==
Version 0.0100, Build 1!
PerlChess consists of two packages and a "main" package which creates a PerlChess object to play the game.
  PerlChess
    ->{"Board"} (an 8x8 array of either "undef" objects or PerlChess::Piece objects)
    ->{"WhiteAttack"} (an 8x8 array of either 0s or numbers corresponding to the sum of 2**value of all white pieces attacking any given square
    ->{"BlackAttack"} (an 8x8 array of same, except for black pieces attacking squares)
    ->{"WhiteMove"} (an 8x8 array of numbers corresponding to sum of 2**values of all white pieces which can legally move to a square)
    ->{"BlackMove"} (same as above except with black pieces)
    ->{"Turn"} (0 for White's turn, 1 for Black's turn)
    ->{"Round"} (starts at 1 and goes up every time it's white's turn)
    ->{"Gameover"} (0 until the game ends, at which point it becomes 1)
    ->new() (creates a new PerlChess object and initializes many things)
    ->pieceByVal($) (returns a PerlChess::Piece object of the piece with that value, or undef if none exists)
    ->attackedBy($$) (returns a list of all pieces of $_[0]'s color threatening cell $_[1].  $_[1] can be 0..63 or e.g. "a4")
    ->movedBy($$) (same as above except ones that can move there, not defending it)
    ->canAttack($) (returns a list of all cells (0..63) that $_[0] (if it's a piece), or the piece ON $_[0] can attack)
    ->moveBoards() (generates $self->{"WhiteMove"} and $self->{"BlackMove"})
    ->AttackBoards() (generates $self->{"WhiteAttack"} and $self->{"BlackAttack"})
    ->mySize() (returns the width and height of the ideal chess square, in characters, based on the terminal size)
    ->showBoard() (prints the playing board onto the terminal window)
    ->getInput() (lets someone play the game by entering commands)
  PerlChess::Piece
    ->{"color"} ("White" or "Black")
    ->{"type"} ("Pawn", "Rook", etc.)
    ->{"spot"} ("a4", "e8", etc.)
    ->{"value"} (a unique number from 1 to 16 is assigned to each piece upon creation (Black pieces are negative) to assist in generating informative attack and move boards)
    ->{"short"} (returns the shortened name, e.g. "WR" (White Rook), "BN" (Black Knight), etc.)
    ->new($$$$) (creates a chess piece of color $_[0], type $_[1], on spot $_[2], with value $_[3])
    ->captured() (sets the spot of the piece to "64", not a valid location)
    ->move($) (changes $self->{"spot"} to $_[0] ((unnececssary??)))
    ->promote($) (changes $self->{"type"} to $_[0], only on Pawns, and only to something other than Pawn or King).

Things to implement:
Check, Checkmate, Stalemate, Castling, En Passant, Move History, Undoing, other notations, panels, options, and a whole mess of other things.
