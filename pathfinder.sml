use "graph.sml";
use "pqueue.sml";
use "queue.sml";

structure Pathfinder =
struct
	datatype Color = White | Gray | Black

	(*
		aStar (grid, start, goal )
		TYPE: (Pathfinder.Color * int * int * (int * int)) Graph.graph * (int * int) * (int * int) -> (int * int) list
		PRE: Both start and goal have to be valid coordinates in grid.
		POST: list containing coordinates(in the form of (x,y)) with a shortest path from start to goal in grid.
		SIDE-EFFECTS: ?? (none since grid is copied? Does that count as a side-effect of itself?)
		EXCEPTIONS: Fail raised if there is no path from start to goal in grid. Subscript raised if either start or goal's not a valid coordinate in grid.
	*)
	fun aStar ( grid', start : (int * int), goal)  =
		let
			val grid = Graph.copy(grid');
			val openList = BinoHeap.insert( BinoHeap.empty, 0,start );
			fun getParent pos = case (Graph.at grid) pos of (SOME (Graph.Node(_, _, (_,_,_, parent )))) => parent
			fun pathfind'  ([]) = raise Fail "Path not found"
			|	pathfind' ( openList ) =
				let
					val ((currentF, currentNode), openList ) = BinoHeap.extractMin( openList );
					fun calculateG ((x, y), (adjX, adjY)) = case ((Graph.at grid) (x, y)) of (SOME (Graph.Node(_, _, (_, g, _, _)))) => if (Int.abs(x - adjX) + Int.abs(y - adjY)) = 2 then g+14 else g+10
					fun calculateH((x,y), (ex, ey)) = 10*(Int.abs(x - ex) + Int.abs(y - ey))
					(* Diagonal heuristic
					fun calculateH((x,y), (ex, ey)) =
						let
							val xDist = Int.abs(x - ex)
							val yDist = Int.abs(y - ey)
						in
							if xDist > yDist then
								14*yDist + 10*(xDist-yDist)
							else
								14*xDist + 10*(yDist-xDist)
						end
					*)

					fun getF pos = case (Graph.at grid) pos of (SOME (Graph.Node(_, _, (_,g,h, _ )))) => g+h

					fun processAdjacent adjPos =
						let
							val (adjl, adjC,adjG,adjH,adjParent) = case (Graph.at grid) adjPos of (SOME (Graph.Node(_, adjl, (c,g,h, parent )))) => (adjl, c,g,h,parent)
						in
							if adjC = Black then
								0
							else if adjC = White then
								((Graph.update grid) (adjPos, (SOME (Graph.Node(adjPos, adjl, (Gray,calculateG(currentNode, adjPos),calculateH(adjPos, goal), currentNode ))))); 1)
							else (*Gray*)
								let
									val newG = calculateG(currentNode, adjPos)
								in
									if newG >= adjG then
										0
									else ((Graph.update grid) (adjPos, (SOME (Graph.Node(adjPos, adjl, (Gray,newG,adjH, currentNode ))))); 2)
								end
						end

					val (_, adjList) = case ((Graph.at grid) currentNode) of (SOME (Graph.Node(tXY, tadjList, (_, g, h, parent)))) => ((Graph.update grid) (currentNode, (SOME (Graph.Node(tXY, tadjList, (Black, g, h, parent))))),tadjList)
				in
					if currentNode = goal then () else
					pathfind' (foldr(fn (adjPos, xs) => case processAdjacent adjPos of
						(0) => xs
						| (1) => BinoHeap.insert(xs, getF adjPos, adjPos)
						| (2) => BinoHeap.update(xs, getF adjPos, adjPos)
						 ) openList adjList)
				end

			fun rewind (pos) =
				let
					val parent = getParent pos;
				in
					if pos = start then [] else pos::rewind parent
				end
		in
			(pathfind' openList; rev (rewind goal))
		end;

	(*
		aStarGraph grid
		TYPE: Grid.grid -> (Pathfinder.Color * int * int * (int * int)) Graph.graph
		PRE: true
		POST: the graph representation of the provided grid that is usable with aStar.
	*)
	fun aStarGraph grid = Graph.make(grid, (White, 0,0,(0,0)));


	(*
		dijkstraGraph grid
		TYPE: Grid.grid -> int option Graph.graph
		PRE: true
		POST: the graph representation of the provided grid that is usable with dijkstra.
	*)
	fun dijkstraGraph grid = Graph.make(grid, NONE : int option);



	(*
		dijkstra (grid, start, goal)
		TYPE: int option Graph.graph * (int * int) * (int * int) -> (int * int) list
		PRE: Both start and goal have to be valid coordinates in grid.
		POST: list containing coordinates(in the form of (x,y)) with a shortest path from start to goal in grid.
		SIDE-EFFECTS: ??
		EXCEPTIONS: Fail raised if there is no path from start to goal in grid. Subscript raised if either start or goal's not a valid coordinate in grid.
	*)
	fun dijkstra ( grid', start : (int * int), goal) =
	let
		val grid = Graph.copy(grid');
		val openList = Queue.enqueue (Queue.empty, start)
		val _ = case (Graph.at grid) start of (SOME (Graph.Node(_, adjList, _))) => (Graph.update grid) (start, SOME (Graph.Node((start, adjList, SOME 0))))


		fun	dijkstra' openList =
			let
				val (currentNode as (currentX, currentY), openList) = if Queue.isEmpty openList then raise Fail "Path not found" else (Queue.head(openList), Queue.dequeue(openList))
				val value = case (Graph.at grid) currentNode of (SOME (Graph.Node(_, _, SOME value))) => value

				fun processAdjacent(adjPos as (adjX, adjY), openList) =
					let
						val (adjV, adjAL) = case (Graph.at grid) adjPos of (SOME (Graph.Node(_, adjAL, adjV))) => (adjV, adjAL)
					in
						if adjV = NONE then
							if (Int.abs(currentX - adjX) + Int.abs(currentY - adjY)) = 2 then
								((Graph.update grid) (adjPos, (SOME (Graph.Node(adjPos, adjAL, SOME (value+14))))); Queue.enqueue(openList, adjPos))
							else
								((Graph.update grid) (adjPos, (SOME (Graph.Node(adjPos, adjAL, SOME (value+10))))); Queue.enqueue(openList, adjPos))
						else
							openList
					end

				val adjList = case (Graph.at grid) currentNode of (SOME (Graph.Node(_, adjList, _))) => adjList
			in
				if currentNode = goal then openList else
				dijkstra' (foldr processAdjacent openList adjList)
			end

		fun rewind pos =
			let
				val (value, adjPos, adjList) = case (Graph.at grid) pos of (SOME (Graph.Node(_, adjPos::adjList, SOME value))) => (value, adjPos, adjList)

				fun compareV ((x, y), (x2,y2) ) = case ((Graph.at grid) (x, y), (Graph.at grid) (x2, y2)) of
												  ((SOME (Graph.Node(_, _, SOME val1))), (SOME (Graph.Node(_, _, SOME val2)))) => if val1 < val2 then (x,y) else (x2,y2)
												  | ((SOME (Graph.Node(_, _, NONE))), (SOME (Graph.Node(_, _, val2)))) => (x2, y2)
												  | ((SOME (Graph.Node(_, _, val1))), (SOME (Graph.Node(_, _, NONE)))) => (x, y)

			in
				if value = 0 then [] else pos::rewind( (foldr compareV adjPos adjList))
			end
	in
		(dijkstra'(openList); rev (rewind goal))
	end

end
