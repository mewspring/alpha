use "preprocess.sml";

datatype color = White | Gray | Black;

structure Graph =
struct

datatype 'a node = Node of (int * int) * (int * int) list * 'a;

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

(*val graph = makeGraph(grid', "test");*)
end

fun pathfind (grid, start as (sx,sy) : (int * int), goal as (ex, ey))  =
	let

		val openList = [(0,start)]

		fun pathfind'  ([]) = raise Fail "Path not found"
		|	pathfind' ((currentF, (currentNode as (x,y)))::openList) =
			let
				fun calculateG ((x, y), (adjX, adjY)) = case (Graph.at(grid, x, y)) of (SOME (Graph.Node(_, _, (_, g, _, _)))) => if (Int.abs(x - adjX) + Int.abs(y - adjY)) = 2 then g+14 else g+10
				fun calculateH(x,y) = 10*(Int.abs(x - ex) + Int.abs(y - ey))

				fun insertInOpen ([], (f, adjXY)) = [(f, adjXY)]
				| insertInOpen ((xF, xXY )::openList, (f, adjXY)) = if f <= xF then (f, adjXY)::(xF, xXY)::openList else (xF, xXY)::insertInOpen(openList, (f, adjXY))

				fun getF(adjX, adjY) = case Graph.at(grid, adjX, adjY) of (SOME (Graph.Node(_, _, (_,g,h, _ )))) => g+h

				fun removeFromOpen([], _) = []
				|	removeFromOpen((f, xy)::openList, delXY) = if delXY = xy then openList else (f, xy)::removeFromOpen(openList, delXY)

				fun hasBestG((currentX, currentY), (adjX, adjY)) = case (Graph.at(grid, currentX, currentY), Graph.at(grid, adjX, adjY)) of ((SOME (Graph.Node(_, _, (_, g, _, _)))), (SOME (Graph.Node(_, _, (_, adjG, _, _))))) => g <= adjG

				fun doStuff (adjX, adjY) =
					let
						val (xy, adjl, adjC,adjG,adjH,adjParent) = case Graph.at(grid, adjX, adjY) of (SOME (Graph.Node(xy, adjl, (c,g,h, parent )))) => (xy, adjl, c,g,h,parent)
					in
						if adjC = Black then (0, NONE)
						else if adjC = White then (Graph.update(grid, adjX, adjY, (SOME (Graph.Node(xy, adjl, (Gray,calculateG(currentNode, (adjX, adjY)),calculateH(adjX, adjY), currentNode ))))); (1, SOME (calculateG(currentNode, (adjX, adjY))+calculateH(adjX, adjY), (adjX, adjY))))
						else if hasBestG(currentNode, (adjX, adjY) ) then (0, NONE) else (Graph.update(grid, adjX, adjY, (SOME (Graph.Node(xy, adjl, (Gray,calculateG(currentNode, (adjX, adjY)),calculateH(adjX, adjY), currentNode ))))); (2, SOME (calculateG(currentNode, (adjX, adjY))+calculateH(adjX, adjY), (adjX, adjY))))
					end

				val (_, adjList) = case Graph.at(grid, x, y) of (SOME (Graph.Node(tXY, tadjList, (_, g, h, parent)))) => (Graph.update(grid, x, y, (SOME (Graph.Node(tXY, tadjList, (Black, g, h, parent))))),tadjList)
			in
				if currentNode = goal then () else
				pathfind' (foldr(fn ((adjX,adjY), xs) => case doStuff(adjX, adjY) of (0, _) => xs | (1, SOME (f, adjXY)) => insertInOpen(xs, (f, adjXY)) | (2, SOME (f, adjXY)) => insertInOpen(removeFromOpen(xs, (adjX, adjY)), (getF(adjX, adjY) ,(adjX, adjY))) ) openList adjList)
			end

			fun rewind (pos as (x,y)) =
				let
					val parent = case Graph.at(grid, x, y) of (SOME (Graph.Node(_, _, (_,_,_, parent )))) => parent
				in
					if pos = start then [] else pos::rewind parent
				end
	in
		(pathfind' openList; rev (rewind goal))
	end

fun pathfinder (start, goal) = pathfind(Graph.makeGraph(grid', (White, 0,0,(0,0))), start, goal)