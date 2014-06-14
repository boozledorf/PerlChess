package PerlChess;
#Make sure things work the way they're supposed to work!;
use strict; use warnings;
#Debugging, easy printing, switch and Perl6::Say should always be included everywhere, and the function "max" is dumb not to be in all languages...
use Data::Dumper; use Perl6::Say; use Switch; use List::Util qw(max);
#dclone lets me deeply copy nested data structures without carrying over any memory references.  chars lets me get the width and height of the terminal in terms of characters.
use Storable qw(dclone); use Term::Size::Any qw(chars);

#Speaks for itself.  Builds refer to the number of times I've, I dunno, submitted to github or something.  Maybe it'll count .exe compiles.
#Build 1 should eventually be when I hope it actually plays some form of chess rather than just showing chess-like things.
my ($version, $builds) = ("0.0100", 0);

#Create a new Chess game
sub new{
	my $class = shift;
	say "Welcome to Perlchess!";
	say "This is version $version, build $builds";
	
	#Initialize the default 8x8 grid of zeroes.
	my @blank = (); push(@blank,[(0) x 8]) foreach 1..8;
	#Set $ChessBoard to one of those 8x8 grids.
	my $ChessBoard = dclone(\@blank);
	#The location of each piece on a chessboard, 0 being lower left and 63 being upper right.  @PieceLocs must be an array of array references.
	#These get split up into rank and file for storage as int(num/8) and num%8, respectively.
	#For black, the rank then becomes 7-int(num/8) rather than doing 63-num for everything as that would switch position of King and Queen (Queen always starts on her color).
	my @PieceLocs = ([8..15],[0,7],[1,6],[2,5],[3],[4]); my @PieceNames = qw/Pawn Rook Knight Bishop Queen King/; #qw/Word1 Word2 Word3/ is basically split(" ","Word1 Word2 Word3")
	#Initialize the chessboard and create the chess pieces.
	my $num = 1;
	my @PieceList = ();
	foreach(0..5){ #In other words, foreach (0..$#PieceLocs)
		foreach my $p(@{$PieceLocs[$_]}){ #Loop $p through each element of the anonymous array stored in memory location $PieceLocs[$_]
			my ($r,$f) = (int($p/8),$p%8); #Rank and File
			#Creates the new chess pieces, storing their color, name, location, and value; top creates white pieces, and bottom creates Black pieces.  Black pieces have "negative" value so the numbers don't get absurdly large later on.
			$ChessBoard->[$r][$f] = PerlChess::Piece->new("White",$PieceNames[$_],"".@{["a".."h"]}[$f].(1+$r),$num);
			$ChessBoard->[7-$r][$f] = PerlChess::Piece->new("Black",$PieceNames[$_],"".@{["a".."h"]}[$f].(8-$r),-$num++);
			push(@PieceList,$ChessBoard->[$r][$f],$ChessBoard->[7-$r][$f]);
		}
	}
	#Create the $self object to be blessed.  $self->{"Board"} will be the $ChessBoard created here and updated as moves are made
	#$self->{"Turn"} will be whose turn it is (0 for White, 1 for Black).  $self->{"Round"} is which turn number it is (1, 2, 5, etc..), and $self->{"GameOver"} is 0 until the game ends.
	my $self = {"Board" => $ChessBoard,"Pieces" => \@PieceList,"Turn" => 0,"Round" => 1,"Gameover" => 0};
	bless $self,$class;
	#Generate the attack boards and the move boards
	($self->{"WhiteAttack"},$self->{"BlackAttack"}) = $self->attackBoards;
	($self->{"WhiteMove"},$self->{"BlackMove"}) = $self->moveBoards;
	return $self;
}
#Returns a piece by the value passed to it (sign sensitive)
sub pieceByVal{
	my $self = shift; my $target = shift;
	my @result = grep{
		my $piece = $self->{"Board"}->[int($_/8)][$_%8];
		(ref($piece) && $piece->{"value"} == $target)
	}(0..63);
	return @result?$self->{"Board"}->[int($result[0]/8)][$result[0]%8]:undef;
}
#Returns a list of pieces of the specified color that are attacking the specified square
#The square can be either a number (0-63) or a square name (e.g. "a4")
sub attackedBy{
	my $self = shift; my $color = shift; my $place = shift;
	my @pieces;
	$place =~ /([a-h])([1-8])/;
	my ($rank,$file) = ($1?(eval($2)-1,ord($1)-97):(int($place/8),$place%8));
	my $value = abs(($color eq "White"?$self->{"WhiteAttack"}:$self->{"BlackAttack"})->[$rank][$file]);
	my $num = 1;
	while ($value){
		my $piece = $self->pieceByVal(($color eq "White"?1:-1)*$num);
		push(@pieces,$piece->{"value"}) if $value&(2**$num);
		$value-=$value%(2**$num);
		$num++;
	}
	return @pieces;
}
#Does the same as attackedBy except with moving to instead
sub movedBy{
	my $self = shift; my $color = shift; my $place = shift;
	my @pieces;
	$place =~ /([a-h])([1-8])/;
	my ($rank,$file) = ($1?(eval($2)-1,ord($1)-97):(int($place/8),$place%8));
	my $value = abs(($color eq "White"?$self->{"WhiteMove"}:$self->{"BlackMove"})->[$rank][$file]);
	my $num = 1;
	while ($value){
		my $piece = $self->pieceByVal(($color eq "White"?1:-1)*$num);
		push(@pieces,$piece->{"value"}) if $value&(2**$num);
		$value-=$value%(2**$num);
		$num++;
	}
	return @pieces;
}
#Generates a list of all cells which the piece on the specified square can move to
#Will accept a piece, a number (0-63) or a chessboard location (e.g. "a4")
sub canAttack{ #Do... something... to make this less ugly, please.
	my $self = shift; my $piece = shift;
	$piece = $piece->{"spot"} if (ref($piece)eq"PerlChess::Piece");
	$piece = 8*eval($2 - 1) + (ord($1)-97) if $piece =~ /([a-h])([1-8])/;
	return (0) if !($piece~~[0..63]);
	my $part = $self->{"Board"}->[int($piece/8)][$piece%8];
	return (0) if !ref($part);
	my $col = $self->{"Board"}->[int($piece/8)][$piece%8]->{"color"}eq"White";
	my $value = $self->{"Board"}->[int($piece/8)][$piece%8]->{"value"};
	my @places = ();
	my $board = $self->{($col?"White":"Black")."Attack"};
	foreach my $rank(0..7){foreach my $file(0..7){push (@places,8*$rank+$file) if(($board->[$rank][$file]) & (2**$value));}}
	return @places;
}
sub moveBoards{
	my $self = shift;
	my @blank = (); push(@blank,[(0) x 8]) foreach 1..8;
	my $WhiteBoard = dclone(\@blank); my $BlackBoard = dclone(\@blank);
	#List of ways each piece can attack.  'n' represent all values from -7 to 7 inclusive, with the exception of 0.  Within a group, all instances of 'n' represent the same value.
	#I left out pawns because the way they MOVE is funky, but the way they attack is straightforward.
	my %MPatterns = ("Queen" => ["0,n","n,0","n,n","n,-n"],"Rook" => ["0,n","n,0"],
		"Knight" => ["2,1","1,2","2,-1","-1,2","-2,1","1,-2","-2,-1","-1,-2"],
		"Bishop" => ["n,n","n,-n"], "King" => ["1,1","1,0","1,-1","0,1","0,-1","-1,1","-1,0","-1,-1"]);
	foreach my $rank (0..7){ #Go through the board and look at all pieces on it
		foreach my $file (0..7){
			my $piece = $self->{"Board"}->[$rank][$file]; #Store the current piece in the $piece variable.
			next unless ref($piece); #Only do things if $piece is a chess piece.  Skip if it isn't.
			my $col = $piece->{"color"}eq"White"; #This piece's color determines a lot of things, so set $col to 1 if $piece->{"color"} is "White", 0 if not.
			#This is a set of instructions that get executed exactly the same way six different times, but all depend on variables in this particular context, so I define it here.
			#The instructions contained in $test test a particular move to see if it puts the player in check.  If it does, then it's not a valid move.  If it doesn't, update the appropriate moveboard.
			#$temp becomes a _copy_ of $self, not another reference to it.
			#Test the move, then check if the position of $piece's color's king is threatened, act accordingly, then restore the original copy of the game to $self.
			my $test = 'my $temp = dclone($self);$self = dclone($temp);
				$self->{"Board"}->[$rn][$fn]->captured if ref($self->{"Board"}->[$rn][$fn]);
				$self->{"Board"}->[$rn][$fn]=$self->{"Board"}->[$rank][$file];
				$self->{"Board"}->[$rank][$file] = undef;
				($self->{"WhiteAttack"},$self->{"BlackAttack"}) = $self->attackBoards;
				my $king = $self->pieceByVal($col?16:-16);
				($col?$WhiteBoard:$BlackBoard)->[$rn][$fn]+=2**abs($piece->{"value"}) unless $self->attackedBy(($col?"Black":"White"),$king->{"spot"});
				$self = dclone($temp);';
			if($piece->{"type"}ne"Pawn"){ #If it isn't a pawn (because they work differently)
				my @patterns = @{$MPatterns{$piece->{"type"}}}; #The list of patterns is the value of %MPatterns whose key is $piece's type
				foreach my $patt (@patterns){ #Go through the list of patterns for this piece type
					if($patt !~ /n/){ #If this pattern does not contain the letter 'n', then it's delta-x and delta-y (relative to the piece's own position)
						my($rn,$fn) = $patt =~ /(-?\d),(-?\d)/; #Set $rn and $fn (rank-new and file-new) to those relative positions
						($rn,$fn) = (($col?1:-1)*$rn+$rank,$fn+$file); #Turn the relatives into absolutes
						next if (!($rn~~[0..7] and $fn~~[0..7]) or ($self->{"Board"}->[$rn][$fn] and ($self->{"Board"}->[$rn][$fn])->{"color"} eq $piece->{"color"})); #Skip this pattern if it falls outside the chess board, or --if there's a piece there AND it's the same color as our piece (we can't capture our own pieces)--.
						eval $test; #Run the checktest.
						}else{ #If this pattern DOES contain the letter 'n', then we've gotta do our iterations.
						my $s = 0; #Set the skip-test to 0 (we're not skipping anything... YET.)
						foreach my $n (1..7,reverse(-7..-1)){#iterate n from 1 to 7, then from -1 to -7.
							($n==-1?$s=0:next) if $s; #If our skip-test is true, then skip, unless we're at our second half, from -1 to -7, in which case we're no longer skipping.
							my ($rn,$fn) = (($patt =~ s/n/($n)/gr) =~ /(.*),(.*)/); #Replace /n/ with /($n)/ (parentheses are necessary to resolve double negatives).  These are our possible moving positions relative to the piece's position.
							($rn,$fn) = ($rank+($col?1:-1)*eval($rn),eval($fn)+$file); #Turn those relatives into absolutes.  The eval turns it from a string into an integer.
							next if !($rn~~[0..7] and $fn~~[0..7]); #Skip this if we're outside our range.
							if($self->{"Board"}->[$rn][$fn] and $self->{"Board"}->[$rn][$fn]->{"color"}eq$piece->{"color"}){$s=1;next;} #Skip the rest until the second half (or until we're done if we're already in the second half) if there is a piece in this specified square of the same color as $piece.
							eval($test); #Run the checktest.
						}
					}
				}
			}else{	#If it is a Pawn
				if(!ref($self->{"Board"}->[$rank+($col?1:-1)][$file])){ #If there is no piece in the square directly in front of the pawn, it can move forward
					my ($rn,$fn) = ($rank+($col?1:-1),$file); #Absolute positions this time, wow!
					if($rn~~[0..7] and $fn~~[0..7] and !$self->{"Board"}->[$rn][$fn]){ #If we're inside the the board and there is no piece there [I can probably get rid of this if statement as this check should never produce false if implemented correctly
						eval($test); #Run the checktest
						if($rn==($col?2:5)){ #Remember, pawns can move forward TWO if it hasn't moved yet (is in its initial row)
							$rn+=($col?1:-1); #Yadda yadda
							eval($test) if(!$self->{"Board"}->[$rn][$fn]);
						}
					}
				}
				#Pawns can move diagonally only for capture
				my ($rn,$fn) = ($rank+($col?1:-1),$file-1); my $loc = $self->{"Board"}->[$rn][$fn]; #
				eval($test) if($rn~~[0..7] and $fn~~[0..7] and $loc and $loc->{"color"}ne$piece->{"color"});
				($rn,$fn) = ($rank+($col?1:-1),$file+1); $loc = $self->{"Board"}->[$rn][$fn];
				eval($test) if($rn~~[0..7] and $fn~~[0..7] and $loc and $loc->{"color"}ne$piece->{"color"});
				#Implement En Passant eventually here..
			}
		}
	}
	return ($WhiteBoard,$BlackBoard);
}
#Generates two attack boards, one for White, one for Black.
#These boards are a lists of which pieces are attacking which squares.
#Each cell contains a number uniquely identifying all pieces which threaten that square.
sub attackBoards{
	my $self = shift;
	my @blank = (); push(@blank,[(0) x 8]) foreach 1..8;
	my $WhiteBoard = dclone(\@blank); my $BlackBoard = dclone(\@blank);
	my %APatterns = ("Queen" => ["0,n","n,0","n,n","n,-n"],"Pawn"=>["1,-1","1,1"],
			"Knight" => ["2,1","1,2","2,-1","-1,2","-2,1","1,-2","-2,-1","-1,-2"],
			"Rook" => ["0,n","n,0"], "Bishop" => ["n,n","n,-n"],
			"King" => ["1,1","1,0","1,-1","0,1","0,-1","-1,1","-1,0","-1,-1"]);
	foreach my $rank (0..7){
		foreach my $file (0..7){
			my $piece = $self->{"Board"}->[$rank][$file];
			if(ref($piece)){
				my $col = $piece->{"color"}eq"White";
				my @patterns = @{$APatterns{$piece->{"type"}}};
				foreach my $patt (@patterns){
					if($patt !~ /n/){
						my ($rn,$fn) = $patt =~ /(-?\d),(-?\d)/;
						($rn,$fn) = (($col?$rn:-$rn) + $rank,$fn + $file);
						next if !($rn~~[0..7] and $fn~~[0..7]);
						($col?$WhiteBoard:$BlackBoard)->[$rn][$fn]|=2**abs($piece->{"value"});
					}else{
						my $s = 0;
						foreach my $n (1..7,reverse(-7..-1)){
							($n==-1?$s=0:next) if $s;
							my ($rn,$fn) = (($patt =~ s/n/($n)/gr) =~ /(.*),(.*)/);
							($rn,$fn) = ($rank + ($col?1:-1)*eval($rn),eval($fn) + $file);
							next if(!($rn~~[0..7]) or !($fn~~[0..7]));
							($col?$WhiteBoard:$BlackBoard)->[$rn][$fn]|=2**abs($piece->{"value"});
							$s = 1 if ($self->{"Board"}->[$rn][$fn]);
						}
					}
				}
			}
		}
	}
	return($WhiteBoard,$BlackBoard);
}
#By default, characters are 8px wide by 12px tall (for Windows cmd, at least).
sub mySize{
	my ($sqwid,$sqhei) = (6,4); #Minimum aspect ratio
	my ($twid,$thei) = chars *STDOUT{IO}; #Gets the width and height of the terminal window.  I think it's magic, but it works (somehow).
	$sqwid = max(6,int(($twid+$twid%2)/16-4)); #Compute the width of our squares in characters(6 or this thing I worked out before and forgot why it's this, but it's this
	$sqhei = int(2*$sqwid/3); #Compute the height of our squares in characters
	return ($sqwid,$sqhei);
}
#Show the board as ascii characters.  Mostly imported from v0.0000;
sub showBoard{
	my $self = shift;
	my ($sqwid,$sqhei) = $self->mySize(); #Get our square height and width
	my @display = (); #Initialize our board display
	my $border = "   ".("+"."-"x$sqwid)x 8 ."+"; #Borders are in the form of +----+----+...+----+ (with number of dashes equal to $sqwid)
	my $middle = "   ".("|"." "x$sqwid)x 8 ."|"; #Middles (non-borders,non-names) are in the form of |    |    |...|    | (with number of spaces equal to $sqwid)
	my $count = ($sqhei-2+$sqhei%2)/2; #The position of the piece shortform or piece color
	push(@display,$border); #Push the first boundary into our display
	foreach my $rank (0..7){ #Loop through the rows of pieces
		my @contents = ($middle) x $sqhei; #Initialize the row contents to all blanks.  We'll only modify the ones we need.
		my $check = ($sqhei >= 4 && $sqwid >= 7); #See if we're big enough for the full display.  If not, we'll use short forms.
		$contents[$count] = " ".($rank+1)." |"; #We're starting basic -- just the rank and the first pipe
		$contents[$count+1] = "   |" if $check; #Same as above except no row number.
		foreach my $file ($self->{"Turn"}?reverse(0..7):(0..7)){ #Loop through each column within this row
			my $piece = $self->{"Board"}->[$rank][$file]; #The piece we're looking at is whatever object is in that cell of the Board.
			my $name; #Initialize $name.
			if($piece){ #If there's a piece there...
				if($check){ #Are we big? Yes!
					$name = $piece->{"color"}." \n"; #Set the color of the piece followed by a space and newline ("White" and "Black" are both five letters)
					my $tsize = length($piece->{"type"}); #Size of the piece type ("Queen" is 5, etc.)
					my $bpad = int((6-$tsize)/2); #Number of spaces at the beginning
					$name.=(" "x$bpad).$piece->{"type"}.(" "x(6-$tsize-$bpad)); #Set the type of the piece with any necessary spaces surrounding it.
				}else{$name = $piece->{"short"}} #Sadly we are not big.  Just use the piece's default {"short"} property.
			}else{$name = ($check?"      \n      ":"  ")} #...but if there isn't, just use empty spaces
			if($check){ #Are we big?  If so, we'll need to pad spaces around our text.  This stuff does that.
				my $spaces = ($sqwid-6-$sqwid%2)/2; $name=~/^(.*)\n(.*)$/;
				$contents[$count].=" "x$spaces.($self->{"Turn"}?$1:$2)." "x($sqwid-$spaces-6)."|"; #The $self->{"Turn"}?$1:$2 is important.  We reverse the board when it's White's turn
				$contents[$count+1].=" "x$spaces.($self->{"Turn"}?$2:$1)." "x($sqwid-$spaces-6)."|"; #...so we want to make sure that always the color of the piece gets displayed first.
			}else{ #If we're not big, we still pad spaces, but only for one row.
				my $spaces = ($sqwid-2-$sqwid%2)/2;
				$contents[$count].=" "x$spaces.$name." "x($sqwid-$spaces-2)."|";
			}
		}
		foreach (@contents){ #Fuck this loop, but it adds "color" to the grid, marking the difference between black and white squares.
			my $line = "";
			my $cell = 0;
			while(/\|([^\|]+)/g){ #Just accept that it does what it's supposed to do.  I hate this already.
				$line.="|";
				if(($cell++ +$rank)%2) {$line.=$1=~s/(\s{3,})/" ".(chr(254) x (length($1)-2))." "/erg}
				else {$line.=$1}
			};
			$_ = substr($_,0,3).$line."|";
		}
		push(@display,@contents,$border); #Push the contents of the row, followed by another border.
	}
	@display = reverse @display unless $self->{"Turn"}; #If it's white's turn, we've accidentally added everything exactly upside-down.  Just reverse the lines and we're good.
	print "\n" x 3; #Print some blank spaces
	say join("\n",@display); #Print the board
	my $spaces = ($sqwid-$sqwid%2)/2-1; #Compute distance between file lettering
	print " " x 4; #Print initial pad
	say map{" "x$spaces.$_." "x($sqwid - $spaces)}($self->{"Turn"}?reverse("a".."h"):("a".."h")); #Print the file lettering
}
#Reads and interprets user input to make moves, change settings, etc.
#Currently I will only be implementing Algebraic Notation.  A future version (probably a minor version such as 0.n or a major version such as n.0) will include support for others.
#Nevertheless, I am attempting to set up the framework for more notations by using a hash.
sub getInput{
	my $self = shift;
	my $turnp = $self->{"Turn"}?"Black":"White"; my $nturnp = $self->{"Turn"}?"White":"Black";
	my $help = "\nCurrent mode is Algebraic Notation (no alternatives exist in current version).
	To enter a move in Algebraic Notation, ";
	my $validated;
	my %notations = ("Algebraic" => qr/^([BKNQR]?)([a-h]?)([1-8]?)([x:]?)([a-h])([1-8])(\+?)$/);
	until($validated){
		print "Enter a move (Algebraic Notation) or a command\n".($self->{"Turn"}?"Black":"White")." $self->{\"Round\"}>: ";
		my $command = <>;chomp $command;
		exit if $command ~~ ["exit","Exit","quit","Quit"];
		say $help if($command =~ /--help/);
		if($command =~ m/$notations{"Algebraic"}/){
			my ($rank,$file) = (eval($6)-1,ord($5)-97);
			my $pt = "$5$6";
			if($4){ #If we specified a capture, make sure there's something to capture there.
				if(!$self->{"Board"}->[$rank][$file]){ #oops..
					say "Nothing to capture on cell $pt";
					next;
				}
			}
			#There has to be a better way of doing this, whichever one I call has the same arguments...
			my @approachers = ($4?$self->attackedBy($turnp,$pt):$self->movedBy($turnp,$pt));
			my @matching = ();
			my %types = ("B" => "Bishop", "K" => "King", "N" => "Knight", "Q" => "Queen", "R" => "Rook");
			foreach my $piece (@approachers){push @matching,$self->pieceByVal($piece) if $self->pieceByVal($piece)->{"type"} eq ($1?$types{$1}:"Pawn")};
			if($#matching==-1){
				say "No specified pieces can move to cell $pt.\nRemember to check your piece prefix (leave it blank for Pawns)";
				$self->pieceByVal($self->{"Turn"}?-16:16)->{"spot"} =~ /([a-h])([1-8])/;
				my ($kingr,$kingf) = (eval($2)-1,ord($1)-97);
				say "(You are in check!)" if $self->{$nturnp."Attack"}->[$kingr][$kingf];
				next
			}elsif($#matching==0){#Only one available piece!
				$self->{"Board"}->[$rank][$file]->captured() if($self->{"Board"}->[$rank][$file]);
				$matching[0]->{"spot"} =~ /([a-h])([1-8])/;
				my($prank,$pfile) = (eval($2)-1,ord($1)-97);
				$matching[0]->{"spot"} = $pt;
				$self->{"Board"}->[$rank][$file] = $matching[0];
				$self->{"Board"}->[$prank][$pfile] = undef;
				
				
				$validated=1;
			}
		}
	}
	$self->showBoard();
	($self->{"WhiteAttack"},$self->{"BlackAttack"}) = $self->attackBoards;
	($self->{"WhiteMove"},$self->{"BlackMove"}) = $self->moveBoards;
	$self->{"Round"}+=$self->{"Turn"};
	$self->{"Turn"}^=1;
	my $gmcheck = 1;
	foreach(0..63){$gmcheck = 0 if$self->{$nturnp."Move"}->[int($_/8)][$_%8];}
	if($gmcheck){
		$self->{"Gameover"} = 1;
		say "Gameover!\n$nturnp is in either checkmate or stalemate and has no legal moves!";
		die;
	}
}
#End package PerlChess
package PerlChess::Piece;
#Yadda yadda
use strict; use warnings; use Data::Dumper; use Perl6::Say; use Switch; use List::Util qw(max);

sub new{
	my $class = shift;
	my ($color, $type, $spot, $val) = @_;
	$type=~/^(.)/;
	my $short = (($color eq "White")?"W":"B").(($type eq "Knight")?"N":$1);
	my $self = {"color" => $color, "type" => $type, "spot" => $spot, "value" => $val,"short" => $short};
	bless $self, $class;
	return $self;
}
sub captured{
	my $self = shift;
	$self->{"spot"} = "64";
}
sub move{
	my $self = shift;
	$self->{"spot"} = shift;
	return $self->{"spot"};
}
sub promote{
	my $self = shift;
	my $ntype = shift;
	return $self->{"type"} if ($self->{"type"} ne "Pawn" || !($ntype ~~ ["Rook","Bishop","Knight","Queen"]));
	$self->{"type"} = $ntype;
	return $self->{"type"};
}

#End package PerlChess::Piece


package main;
#Yadda yadda
use strict; use warnings; use Data::Dumper; use Perl6::Say; use Switch; use List::Util qw(max);

my $Chess = PerlChess->new();
$Chess->showBoard();
$Chess->getInput() foreach (0..10);

END {print "Goodbye\nThank you for playing PerlChess."}