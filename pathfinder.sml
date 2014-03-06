use "graph.sml";
use "pqueue.sml";
use "queue.sml";

structure Pathfinder =
struct
	val processedNodes = ref 0;
	fun resetProcessedNodes() = processedNodes := 0
	fun incrementProcessedNodes() = processedNodes := !processedNodes + 1
	(*
		DATATYPE REPRESENTATION:
			Colors are used to track the state of a node in a graph. White
			corresponds to unprocessed nodes, Gray to nodes which are in the open
			list and are not yet fully processed and Black to fully processed
			nodes.
		DATATYPE CONVENTION:
			Nodes should have a color based on its state as mentioned in the
			datatype representation.
	*)
	datatype Color = White | Gray | Black

	(*
		aStar (graph, start, goal )
		TYPE: (Pathfinder.Color * int * int * (int * int)) Graph.graph * (int * int) * (int * int) -> (int * int) list option
		PRE: Both start and goal have to be valid coordinates in graph.
		POST: SOME list containing coordinates(in the form of (x,y)) with a shortest path from start to goal in graph. NONE if there is no path.
		EXCEPTIONS: Subscript raised if either start or goal's not a valid coordinate in graph.
	*)
	fun aStar ( graph', start : (int * int), goal)  =
		let
			val _ = resetProcessedNodes();
			val graph = Graph.copy(graph');
			val openList = Pqueue.insert( Pqueue.empty, 0,start );

			(*
				pathfind' openList
				TYPE: int * (int * int) -> (int * int) Pqueue.binoTree list option
				PRE: start(aStar) and goal(aStar) have to be valid coordinates in graph(in aStar).
				POST: SOME modified openList(see documentation) if there is a shortest path from start(aStar) to goal(aStar). NONE, otherwise.
				VARIANT: length of openList
			*)
			fun pathfind'  ([]) = NONE
			|	pathfind' ( openList ) =
				let
					val ((currentF, currentNode), openList ) = Pqueue.extractMin( openList );
					val _ = incrementProcessedNodes();

					(*
						calculateG (pos, adjPos)
						TYPE: (int * int) * (int * int) -> int
						PRE: pos has to be a valid coordinate in graph(in aStar)
						POST: the g value(see documentation) of adjPos in graph(aStar).
					*)
					fun calculateG ((x, y), (adjX, adjY)) = case ((Graph.at graph) (x, y)) of (SOME (Graph.Node(_, _, (_, g, _, _)))) => if (Int.abs(x - adjX) + Int.abs(y - adjY)) = 2 then g+14 else g+10

					(*
						calculateH (pos, goal)
						TYPE: (int * int) * (int * int) -> int
						PRE: true
						POST: the h value(see documentation) of pos.
					*)
					(* Manhattan distance heuristics *)
					(*fun calculateH((x,y), (ex, ey)) = 10*(Int.abs(x - ex) + Int.abs(y - ey))*)
					(* Diagonal heuristics *)
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

					(*
						getF pos
						TYPE: (int * int) -> int
						PRE: pos has to be a valid coordinate in graph(in aStar).
						POST: the h value(g+h) of pos in graph(aStar).
					*)
					fun getF pos = case (Graph.at graph) pos of (SOME (Graph.Node(_, _, (_,g,h, _ )))) => g+h

					(*
						processAdjacent adjPos
						TYPE: (int * int) -> int
						PRE: adjPos has to be a valid coordinate in graph(in aStar).
						POST: 0 if the color of adjPos in graph(aStar) is Black, or if the g value of adjPos in graph is bigger than or equal
							  the new g value(see documentation) of currentNode(in pathfind'). 1 if the color of adjPos in graph is White and 2 otherwise.
					*)
					fun processAdjacent adjPos =
						let
							val (adjl, adjC,adjG,adjH,adjParent) = case (Graph.at graph) adjPos of (SOME (Graph.Node(_, adjl, (c,g,h, parent )))) => (adjl, c,g,h,parent)
						in
							if adjC = Black then
								0
							else if adjC = White then
								((Graph.update graph) (adjPos, (SOME (Graph.Node(adjPos, adjl, (Gray,calculateG(currentNode, adjPos),calculateH(adjPos, goal), currentNode ))))); 1)
							else (*Gray*)
								let
									val newG = calculateG(currentNode, adjPos)
								in
									if newG >= adjG then
										0
									else ((Graph.update graph) (adjPos, (SOME (Graph.Node(adjPos, adjl, (Gray,newG,adjH, currentNode ))))); 2)
								end
						end

					val (_, adjList) = case ((Graph.at graph) currentNode) of (SOME (Graph.Node(tXY, tadjList, (_, g, h, parent)))) => ((Graph.update graph) (currentNode, (SOME (Graph.Node(tXY, tadjList, (Black, g, h, parent))))),tadjList)
				in
					if currentNode = goal then SOME openList else
					pathfind' (foldr(fn (adjPos, xs) => case processAdjacent adjPos of
						(0) => xs
						| (1) => Pqueue.insert(xs, getF adjPos, adjPos)
						| (2) => Pqueue.update(xs, getF adjPos, adjPos)
						 ) openList adjList)
				end

			(*
				rewind pos
				TYPE: (int * int) -> (int * int) list
				PRE: There must exist at least one valid shortest path from start(in aStar) to goal(aStar) in graph(aStar).
					 pos has to be the same coordinate as goal(in aStar) when calling rewind.
				POST: A list containing all the coordinates in a shortest path from start to goal in graph.
				VARIANT: pos = start
			*)
			fun rewind (pos) =
				let
					val parent = case (Graph.at graph) pos of (SOME (Graph.Node(_, _, (_,_,_, parent )))) => parent
				in
					if pos = start then [] else pos::rewind parent
				end
		in
			if isSome(pathfind' openList) then SOME (rev (rewind goal)) else NONE
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
	fun dijkstra ( graph', start : (int * int), goal) =
	let
		val _ = resetProcessedNodes();
		val graph = Graph.copy(graph');
		val openList = Queue.enqueue (Queue.empty, start)
		val _ = case (Graph.at graph) start of (SOME (Graph.Node(_, adjList, _))) => (Graph.update graph) (start, SOME (Graph.Node((start, adjList, SOME 0))))

		(*
			dijkstra' openList
			TYPE: (int * int) queue -> (int * int) queue option
			PRE: start(dijkstra) and goal(dijkstra) have to be valid coordinates in graph(in dijkstra).
			POST: SOME modified openList(see documentation) if there is a shortest path from start(dijkstra) to goal(dijkstra). NONE, otherwise.
			VARIANT: length of openList
		*)
		fun	dijkstra' openList =
			if Queue.isEmpty openList then NONE else
			let
				val (currentNode as (currentX, currentY), openList) = (Queue.head(openList), Queue.dequeue(openList))
				val _ = incrementProcessedNodes();
				val value = case (Graph.at graph) currentNode of (SOME (Graph.Node(_, _, SOME value))) => value

				(*
					processAdjacent (adjPos, openList)
					TYPE: (int * int ) * int option list -> int option list
					PRE: adjPos has to be a valid coordinate in graph(in dijkstra).
					POST: openList with adjPos added to its tail(enqueued) if the value(see documentation) of adjPos in graph(dijkstra) is NONE; openList otherwise.
					EXCEPTIONS: Fail raised if there is no path from start to goal in graph.
					VARIANT: length of openList
				*)
				fun processAdjacent(adjPos as (adjX, adjY), openList) =
					let
						val (adjV, adjAL) = case (Graph.at graph) adjPos of (SOME (Graph.Node(_, adjAL, adjV))) => (adjV, adjAL)
						val newValue = if (Int.abs(currentX - adjX) + Int.abs(currentY - adjY)) = 2 then
								value+14
							else
								value+10
					in
						if adjV = NONE orelse valOf adjV > newValue then
							((Graph.update graph) (adjPos, (SOME (Graph.Node(adjPos, adjAL, SOME (newValue))))); Queue.enqueue(openList, adjPos))
						else
							openList
					end

				val adjList = case (Graph.at graph) currentNode of (SOME (Graph.Node(_, adjList, _))) => adjList
			in
				if currentNode = goal then SOME openList else
				dijkstra' (foldr processAdjacent openList adjList)
			end

		(*
			rewind pos
			TYPE: (int * int) -> (int * int) list
			PRE: There must exist at least one valid shortest path from start(in dijkstra) to goal(dijkstra) in graph(dijkstra).
				pos has to be the same coordinate as goal when calling rewind.
			POST: A list containing all the coordinates in a shortest path from start to goal in graph.
			VARIANT: value = 0
		*)
		fun rewind pos =
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
				if value = 0 then [] else pos::rewind( (foldr compareV adjPos adjList))
			end
	in
		if isSome(dijkstra'(openList)) then SOME (rev (rewind goal)) else NONE
	end

end
