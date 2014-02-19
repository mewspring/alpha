PolyML.print_depth 100;

fun readLines s =
	case TextIO.inputLine s of
	    NONE => (TextIO.closeIn s; [])
	  | SOME(line) => line::readLines s;

fun getIntList s =
	List.mapPartial (fn x => Int.fromString(Substring.string(x))) (Substring.fields (fn x => x = #",") (Substring.full s));
	(*List.mapPartial Int.fromString (Substring.string(Substring.fields (fn x => x = #",") (Substring.full s)));*)

fun readTmx path =
		List.map getIntList (readLines(TextIO.openIn path));

val s = readTmx "in.txt";

(* int list list * int list -> bool list list *)
fun preprocess () = raise Fail "foo";

(* bool list list -> nodetree *)
fun makeNodeTree () = raise Fail "bar";

(* nodetree -> node *)
fun getNode () = raise Fail "qux";

(* nodetree ->  *)
fun makePathFinder () = raise Fail "a";

(* (int * int) * (int * int) -> (int * int) list *)
fun findPath (start, goal) = raise Fail "baz";
