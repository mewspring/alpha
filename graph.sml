use "preprocess.sml";

datatype color = White | Gray | Black;

datatype node = Node of (int * int) * (int * int) list * color ref;

(*
	isValid (grid, x, y)
	TYPE: bool Array2.array * int * int -> bool
	PRE:
	POST:
*)
fun isValid (grid, x, y) =
	x >= 0 andalso x < Array2.nCols(grid) andalso y >= 0 andalso y < Array2.nRows(grid);

(*
	canWalk (grid, x, y)
	TYPE: bool Array2.array * int * int -> bool
	PRE:
	POST:
*)
fun canWalk (grid, x, y) =
	isValid(grid, x, y) andalso Array2.sub(grid, x, y);

(*
	getAdjacent (grid, x, y)
	TYPE: bool Array2.array * int * int -> (int * int) list
	PRE:
	POST:
*)
(* TODO: prevent walks through diagonal walls. *)
fun getAdjacent (grid, x, y) =
	let
		(*
			getWalk (x, y)
			TYPE: int * int -> (int * int) option
			PRE:
			POST:
		*)
		fun getWalk (x, y) =
			if canWalk(grid, x, y) then
				SOME (x, y)
			else
				NONE
	in
		List.mapPartial getWalk [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
	end;

(*
	getNode (grid, x, y)
	TYPE: bool Array2.array * int * int -> node option
	PRE:
	POST:
*)
fun getNode (grid, x, y) =
	if canWalk(grid, x, y) then
		SOME (Node((x, y), getAdjacent(grid, x, y), ref White))
	else
		NONE;

abstype graph = Graph of node option Array2.array
with
(*
	makeGrid grid
	TYPE: bool Array2.array -> node option Array2.array
	PRE:
	POST:
 *)
fun makeGraph grid =
	let
		val graph = Array2.array(Array2.nRows grid, Array2.nCols grid, NONE)
		fun f (x, y, _) = getNode(grid, x, y)
		val range = {base = graph, row = 0, col = 0, nrows = NONE, ncols = NONE}
		val _ = Array2.modifyi Array2.RowMajor f range
	in
		Graph graph
	end;
end;

val graph = makeGraph(grid');
