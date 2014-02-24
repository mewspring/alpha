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

(* int list list * int list -> bool Array2.array *)
fun preprocess (grid, walkable) =
	Array2.fromList( List.map (fn l => (List.map (fn x=> contains(walkable,x) ) ) l ) grid )
	
datatype state = WHITE | GREY | BLACK
datatype node = Node of (int*int) * (int*int) list * state ref
abstype graph = Graph of node option Array2.array
with

(* Run f with (x,y) index of each item in a *)
fun iteri a f =
	let
		val maxX = Array2.nRows(a);
		val maxY = Array2.nCols(a);
		fun iteri' ( x, y ) = 
			if x = maxX then [] else
				if y = maxY then
					iteri'( x+1, 0)
				else
					(f((x,y));iteri'(x,y+1))
	in
		iteri' ( 0, 0 )
	end

fun new( b ) =
	let
		val nRows = Array2.nRows(b)
		val nCols = Array2.nCols(b)

		val g = Array2.array( nRows, nCols, NONE)
		fun addNode b (r, c) = 
			let
				val currentCell = Array2.sub(b,r,c);
				fun inside ( r, c) = r >= 0 andalso r<nRows andalso c >= 0 andalso c < nCols
				val adjecent = foldl (fn (x as (r2,c2),y) => if (inside x) andalso Array2.sub(b,r2,c2) then (c2,r2)::y else y) [] [ (r-1,c-1),(r-1,c),(r-1,c+1),(r,c-1),(r+1,c-1),(r,c+1),(r+1,c),(r+1,c+1) ]
			in
				if currentCell then
					Array2.update( g, r, c, SOME(Node( (c,r), adjecent, ref WHITE ) ) )
				else
					()
			end
	in
		(iteri g (addNode b); Graph(g))
	end
end

(*Complex test*)

val grid = readGrid ("floor.txt", "objects.txt");
val walkable = readWalkable "walk.txt";
val grid' = preprocess (grid, walkable);


(*simple test*)
(*
val grid = [
			[0,1,0,0],
			[0,0,1,1],
			[1,0,0,0]
			];
val walkable = [0];

val grid' = preprocess (grid, walkable);
*)

(* nodetree -> node *)
fun getNode () = raise Fail "qux";

(* nodetree ->  *)
fun makePathFinder () = raise Fail "a";

(* (int * int) * (int * int) -> (int * int) list *)
fun findPath (start, goal) = raise Fail "baz";

val navgraph = new(grid');