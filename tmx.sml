PolyML.print_depth 100;

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

(* int list list * int list -> bool list list *)
fun preprocess (grid, walkable) =
	List.map (fn l => (List.map (fn x => contains (walkable, x)) l)) grid;

val grid = readGrid ("floor.txt", "objects.txt");
val walkable = readWalkable "walk.txt";
val grid' = preprocess (grid, walkable);

(* bool list list -> nodetree *)
fun makeNodeTree () = raise Fail "bar";

(* nodetree -> node *)
fun getNode () = raise Fail "qux";

(* nodetree ->  *)
fun makePathFinder () = raise Fail "a";

(* (int * int) * (int * int) -> (int * int) list *)
fun findPath (start, goal) = raise Fail "baz";
