use "graph.sml";
use "pqueue.sml";
(*use "pqueue_list.sml";*)
use "queue.sml";

structure Pathfinder =
struct
	datatype Color = White | Gray | Black

	(*
		aStar (graph, start, goal )
		TYPE: (Pathfinder.Color * int * int * (int * int)) Graph.graph * (int * int) * (int * int) -> (int * int) list option
		PRE: Both start and goal have to be valid coordinates in graph.
		POST: SOME list containing coordinates(in the form of (x,y)) with a shortest path from start to goal in graph. NONE if there is no path.
		EXCEPTIONS: Subscript raised if either start or goal's not a valid coordinate in graph.
	*)
	fun aStar ( graph, start : (int * int), goal)  =
		let
			val openList = Pqueue.insert( Pqueue.empty, 0,start );

			(*
				pathfind' (graph, openList)
				TYPE: ((Pathfinder.Color * int * int * (int * int)) Graph.graph) * (int * (int * int)) -> (Pathfinder.Color * int * int * (int * int)) Graph.graph * (int * int) Pqueue.binoTree list option
				PRE: start(aStar) and goal(aStar) have to be valid coordinates in graph(in aStar).
				POST: Graph modified(see documentation) and SOME modified openList(see documentation) if there is a shortest path from start(aStar) to goal(aStar). NONE, otherwise.
				VARIANT: length of openList
			*)
			fun pathfind'  ([], graph) = (graph, NONE)
			|	pathfind' ( openList, graph ) =
				let
					val ((currentF, currentNode), openList ) = Pqueue.extractMin( openList );

					(*
						calculateG (pos, adjPos, graph)
						TYPE: (int * int) * (int * int) * (Pathfinder.Color * int * int * (int * int)) Graph.graph -> int
						PRE: pos has to be a valid coordinate in graph(in aStar)
						POST: the g value(see documentation) of adjPos in graph(aStar).
					*)
					fun calculateG ((x, y), (adjX, adjY), graph) = case ((Graph.at graph) (x, y)) of (SOME (Graph.Node(_, _, (_, g, _, _)))) => if (Int.abs(x - adjX) + Int.abs(y - adjY)) = 2 then g+14 else g+10

					(*
						calculateH (pos, goal, graph)
						TYPE: (int * int) * (int * int) * (Pathfinder.Color * int * int * (int * int)) Graph.graph -> int
						PRE: true
						POST: the h value(see documentation) of pos.
					*)
					(* Manhattan distance heuristics *)
					(*fun calculateH((x,y), (ex, ey), graph) = 10*(Int.abs(x - ex) + Int.abs(y - ey))*)
					(* Diagonal heuristics *)
					fun calculateH((x,y), (ex, ey), graph) =
						let
							val xDist = Int.abs(x - ex)
							val yDist = Int.abs(y - ey)
						in
							if xDist > yDist then
								14*yDist + 10*(xDist-yDist)
							else
								14*xDist + 10*(yDist-xDist)
						end

					(*
						getF (pos, graph)
						TYPE: (int * int) * (Pathfinder.Color * int * int * (int * int)) Graph.graph -> int
						PRE: pos has to be a valid coordinate in graph(in aStar).
						POST: the h value(g+h) of pos in graph(aStar).
					*)
					fun getF (pos, graph) = case (Graph.at graph) pos of (SOME (Graph.Node(_, _, (_,g,h, _ )))) => g+h

					(*
						processAdjacent (adjPos, graph)
						TYPE: (int * int) * (Pathfinder.Color * int * int * (int * int)) Graph.graph -> (Pathfinder.Color * int * int * (int * int)) Graph.graph * int
						PRE: adjPos has to be a valid coordinate in graph(in aStar).
						POST: 0 if the color of adjPos in graph(aStar) is Black, or if the g value of adjPos in graph is bigger than or equal
							  the new g value(see documentation) of currentNode(in pathfind'). 1 if the color of adjPos in graph is White and 2 otherwise.
					*)
					fun processAdjacent (adjPos, graph) =
						let
							val (adjl, adjC,adjG,adjH,adjParent) = case (Graph.at graph) adjPos of (SOME (Graph.Node(_, adjl, (c,g,h, parent )))) => (adjl, c,g,h,parent)
						in
							if adjC = Black then
								(graph,0)
							else if adjC = White then
								((Graph.update graph) (adjPos, (SOME (Graph.Node(adjPos, adjl, (Gray,calculateG(currentNode, adjPos, graph),calculateH(adjPos, goal, graph), currentNode ))))), 1)
							else (*Gray*)
								let
									val newG = calculateG(currentNode, adjPos, graph)
								in
									if newG >= adjG then
										(graph,0)
									else ((Graph.update graph) (adjPos, (SOME (Graph.Node(adjPos, adjl, (Gray,newG,adjH, currentNode ))))), 2)
								end
						end

		 			val (graph, adjList) = case ((Graph.at graph) currentNode) of (SOME (Graph.Node(tXY, tadjList, (_, g, h, parent)))) => ((Graph.update graph) (currentNode, (SOME (Graph.Node(tXY, tadjList, (Black, g, h, parent))))),tadjList)
				in
					if currentNode = goal then (graph, SOME openList) else
					pathfind' (foldr(fn (adjPos, (xs, graph)) => case processAdjacent (adjPos, graph) of
						(graph, 0) => (xs,graph)
						| (graph, 1) => (Pqueue.insert(xs, getF (adjPos, graph), adjPos), graph)
						| (graph, 2) => (Pqueue.update(xs, getF (adjPos, graph), adjPos), graph)
						 ) (openList, graph) adjList)
				end

			(*
				rewind (pos, graph)
				TYPE: (int * int) * (Pathfinder.Color * int * int * (int * int)) Graph.graph -> (int * int) list
				PRE: There must exist at least one valid shortest path from start(in aStar) to goal(aStar) in graph(aStar).
					 pos has to be the same coordinate as goal(in aStar) when calling rewind.
				POST: A list containing all the coordinates in a shortest path from start to goal in graph.
				VARIANT: pos = start
			*)
			fun rewind (pos, graph) =
				let
					val parent = case (Graph.at graph) pos of (SOME (Graph.Node(_, _, (_,_,_, parent )))) => parent
				in
					if pos = start then [] else pos::rewind (parent, graph)
				end
			val (graph, openList) = pathfind' (openList, graph)
		in
			if isSome openList then SOME (rev (rewind (goal, graph))) else NONE
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
		dijkstra (graph, start, goal)
		TYPE: int option Graph.graph * (int * int) * (int * int) -> (int * int) list option
		PRE: Both start and goal have to be valid coordinates in graph.
		POST: SOME list containing coordinates(in the form of (x,y)) with a shortest path from start to goal in graph. NONE, otherwise.
		EXCEPTIONS: Subscript raised if either start or goal's not a valid coordinate in graph.
	*)
	fun dijkstra ( graph, start : (int * int), goal) =
	let
		val openList = Queue.enqueue (Queue.empty, start)
		val graph = case (Graph.at graph) start of (SOME (Graph.Node(_, adjList, _))) => (Graph.update graph) (start, SOME (Graph.Node((start, adjList, SOME 0))))

		(*
			dijkstra' (graph, openList)
			TYPE: (Pathfinder.Color * int * int * (int * int)) Graph.graph * (int * int) queue -> (Pathfinder.Color * int * int * (int * int)) Graph.graph * (int * int) queue option
			PRE: start(dijkstra) and goal(dijkstra) have to be valid coordinates in graph(in dijkstra).
			POST: graph modified(see documentation) and SOME modified openList(see documentation) if there is a shortest path from start(dijkstra) to goal(dijkstra). NONE, otherwise.
			VARIANT: length of openList
		*)
		fun	dijkstra' (graph, openList) =
			if Queue.isEmpty openList then (graph, NONE) else
			let
				val (currentNode as (currentX, currentY), openList) = (Queue.head(openList), Queue.dequeue(openList))
				val value = case (Graph.at graph) currentNode of (SOME (Graph.Node(_, _, SOME value))) => value

				(*
					processAdjacent (adjPos, (graph, openList)
					TYPE: (int * int ) * ((Pathfinder.Color * int * int * (int * int)) Graph.graph * int option list) -> ((Pathfinder.Color * int * int * (int * int)) Graph.graph) * int option list
					PRE: adjPos has to be a valid coordinate in graph(in dijkstra).
					POST: graph modified(see documentation) and openList with adjPos added to its tail(enqueued) if the value(see documentation) of adjPos in graph(dijkstra) is NONE; openList otherwise.
					EXCEPTIONS: Fail raised if there is no path from start to goal in graph.
					VARIANT: length of openList
				*)
				fun processAdjacent(adjPos as (adjX, adjY), (graph, openList)) =
					let
						val (adjV, adjAL) = case (Graph.at graph) adjPos of (SOME (Graph.Node(_, adjAL, adjV))) => (adjV, adjAL)
						val newValue = if (Int.abs(currentX - adjX) + Int.abs(currentY - adjY)) = 2 then
								value+14
							else
								value+10
					in
						if adjV = NONE orelse valOf adjV > newValue then
							((Graph.update graph) (adjPos, (SOME (Graph.Node(adjPos, adjAL, SOME (newValue))))), Queue.enqueue(openList, adjPos))
						else
							(graph, openList)
					end

				val adjList = case (Graph.at graph) currentNode of (SOME (Graph.Node(_, adjList, _))) => adjList
			in
				if currentNode = goal then (graph,SOME openList) else
				dijkstra' (foldr processAdjacent (graph, openList) adjList)
			end

		(*
			rewind (pos, graph)
			TYPE: (int * int) * (Pathfinder.Color * int * int * (int * int)) Graph.graph -> (int * int) list
			PRE: There must exist at least one valid shortest path from start(in dijkstra) to goal(dijkstra) in graph(dijkstra).
				pos has to be the same coordinate as goal when calling rewind.
			POST: A list containing all the coordinates in a shortest path from start to goal in graph.
			VARIANT: value = 0
		*)
		fun rewind (pos, graph) =
			let
				val (value, adjPos, adjList) = case (Graph.at graph) pos of (SOME (Graph.Node(_, adjPos::adjList, SOME value))) => (value, adjPos, adjList)

				(*
					compareV pos1 pos2
					TYPE: (int * int) * (int * int) -> (int * int)
					PRE: both pos1 and pos2 have to be valid coordinates in graph(in dijkstra).
					POST: The coordinate(pos1 or pos2) with the lowest value(see dijkstra) in graph.
						   pos1 if value of pos1 is lower than value of pos2 in graph; pos2 otherwise.
				*)
				fun compareV ((x, y), (x2,y2) ) = case ((Graph.at graph) (x, y), (Graph.at graph) (x2, y2)) of
												  ((SOME (Graph.Node(_, _, SOME val1))), (SOME (Graph.Node(_, _, SOME val2)))) => if val1 < val2 then (x,y) else (x2,y2)
												  | ((SOME (Graph.Node(_, _, NONE))), (SOME (Graph.Node(_, _, val2)))) => (x2, y2)
												  | ((SOME (Graph.Node(_, _, val1))), (SOME (Graph.Node(_, _, NONE)))) => (x, y)

			in
				if value = 0 then [] else pos::rewind( (foldr compareV adjPos adjList), graph)
			end
			val (graph ,openList) = dijkstra' (graph, openList)
	in
		if isSome openList then SOME (rev (rewind (goal, graph))) else NONE
	end

end
