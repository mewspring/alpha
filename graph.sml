structure Graph =
	struct

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

	datatype 'a node = Node of (int * int) * (int * int) list * 'a;
	(*
		getNode (grid, x, y)
		TYPE: bool Array2.array * int * int * 'a-> node option
		PRE:
		POST:
	*)
	fun getNode (grid, x, y, data) =
		if canWalk(grid, x, y) then
			SOME (Node((x, y), getAdjacent(grid, x, y), data))
		else
			NONE;

	abstype 'a graph = Graph of 'a node option Array2.array
	with
	(*
		makeGrid grid
		TYPE: bool Array2.array ->' Graph
		PRE:
		POST:
	 *)
	fun at(Graph grid, x, y) = Array2.sub(grid, x, y)

	fun update(Graph grid, x, y, newData) = Array2.update(grid, x, y, newData)

	fun copy( Graph grid ) =
		let
			val copyArray = Array2.array( Array2.nRows(grid), Array2.nCols(grid), NONE );
			val range = {base = grid, row = 0, col = 0, nrows = NONE, ncols = NONE};
			val _ = Array2.copy { src=range, dst=copyArray, dst_row=0, dst_col=0 };
		in
			Graph copyArray
		end

	fun makeGraph (grid, data) =
		let


			val graph = Array2.array(Array2.nRows grid, Array2.nCols grid, NONE)
			fun f (x, y, _) = getNode(grid, x, y, data)
			val range = {base = graph, row = 0, col = 0, nrows = NONE, ncols = NONE}
			val _ = Array2.modifyi Array2.RowMajor f range
		in
			Graph graph
		end;
	end;
end;
