(*PolyML.print_depth 100*)
Control.Print.printDepth := 100; Control.Print.linewidth := 150;(*SML/NJ*)

datatype state = WHITE | GREY | BLACK
datatype node = Node of (int*int) * (int*int) list * state

fun readLines s =
	case TextIO.inputLine s of
	    NONE => (TextIO.closeIn s; [])
	  | SOME(line) => line::readLines s;

fun getIntList s =
	List.mapPartial (fn x => Int.fromString(Substring.string(x))) (Substring.fields (fn x => x = #",") (Substring.full s));

fun readGrid (floorPath, objectsPath) =
	let
		val floor = List.map getIntList (readLines(TextIO.openIn floorPath))
		val objects = List.map getIntList (readLines(TextIO.openIn objectsPath))
	in
		ListPair.map (fn (fl, ol) => ListPair.map (fn (f, obj) => if obj <> 0 then obj else f) (fl, ol)) (floor, objects)
	end;

fun readWalkable path =
	getIntList (hd(readLines(TextIO.openIn path)));

fun contains (l, x) =
	List.exists (fn y => y = x) l;

(* int list list * int list -> node option list list *)
fun preprocess (grid, walkable) =
	let
		fun preprocessCell( [], _, _ ) = []
		|	preprocessCell( cell::cells, x , y ) =
			if contains (walkable, cell) then 
				SOME( Node( (x,y), [], WHITE))::preprocessCell( cells, x+1, y)
			else
				NONE::preprocessCell( cells, x+1, y)
		fun preprocessRow( [], _ ) = []
		|	preprocessRow( row::rows, y ) =
			preprocessCell( row, 0, y )::preprocessRow( rows, y+1)
			
	in
		preprocessRow(grid, 0)
	end
	
(*Complex test*)
(*
val grid = readGrid ("floor.txt", "objects.txt");
val walkable = readWalkable "walk.txt";
val grid' = preprocess (grid, walkable);
*)

(*simple test*)
val grid = [
			[0,1,0,0],
			[0,0,1,1],
			[1,0,0,0]
			];
val walkable = [0];
val grid' = preprocess (grid, walkable);

(* nodetree -> node *)
fun getNode () = raise Fail "qux";

(* nodetree ->  *)
fun makePathFinder () = raise Fail "a";

(* (int * int) * (int * int) -> (int * int) list *)
fun findPath (start, goal) = raise Fail "baz";

(* bool list list -> nodetree *)
fun linkNodes ( grid ) = 
	let
		fun link( SOME(Node((x,y), links, state)), SOME(Node((x2,y2), _, _)) ) = SOME(Node((x,y), (x2,y2)::links, state))
		|	link( x, y ) = x

		fun iterCells( [], _ ) = []
		|	iterCells( x, [] ) = x
		|	iterCells( curSource::sources, curTarget::targets ) = 
				link(curSource,curTarget)::iterCells( sources, targets )

		fun iterCells'( x::xs, ys ) =
				iterCells( iterCells( x::iterCells(xs, ys), ys ), List.drop( ys, 1) )

		fun iterCells''( x::xs ) =
			let
				val (hd::tail) = iterCells( x::xs, xs )
			in
				hd::iterCells( tail, x::xs)
			end

		fun iterRows( [] ) = []
		|	iterRows( curRow::[] ) =
				[iterCells''(curRow)]
		|	iterRows( curRow::nextRow::rows ) =
				iterCells''(iterCells'( curRow, nextRow ))::iterRows( iterCells'( nextRow, curRow )::rows )
	in
		iterRows( grid )
	end

val nodeTree = linkNodes( grid' )