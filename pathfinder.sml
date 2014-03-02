use "graph.sml";
use "pqueue.sml";
use "queue.sml";

structure Pathfinder =
struct
	datatype Color = White | Gray | Black
	fun aStar ( grid', start as (sx,sy) : (int * int), goal as (ex, ey))  =
		let
			val grid = Graph.copy(grid');
			val openList = BinoHeap.insert( BinoHeap.empty, 0,start );
			fun getParent(x,y) = case (Graph.at grid) (x, y) of (SOME (Graph.Node(_, _, (_,_,_, parent )))) => parent
			fun pathfind'  ([]) = raise Fail "Path not found"
			|	pathfind' ( openList ) =
				let
					val ((currentF, (currentNode as (x,y))), openList ) = BinoHeap.extractMin( openList );
					fun calculateG ((x, y), (adjX, adjY)) = case ((Graph.at grid) (x, y)) of (SOME (Graph.Node(_, _, (_, g, _, _)))) => if (Int.abs(x - adjX) + Int.abs(y - adjY)) = 2 then g+14 else g+10
					fun calculateH(x,y) = 10*(Int.abs(x - ex) + Int.abs(y - ey))

					fun getF(adjX, adjY) = case (Graph.at grid) (adjX, adjY) of (SOME (Graph.Node(_, _, (_,g,h, _ )))) => g+h

					fun doStuff (adjX, adjY) =
						let
							val (xy, adjl, adjC,adjG,adjH,adjParent) = case (Graph.at grid) (adjX, adjY) of (SOME (Graph.Node(xy, adjl, (c,g,h, parent )))) => (xy, adjl, c,g,h,parent)
						in
							if adjC = Black then
								0
							else if adjC = White then
								((Graph.update grid) ((adjX, adjY), (SOME (Graph.Node(xy, adjl, (Gray,calculateG(currentNode, (adjX, adjY)),calculateH(adjX, adjY), currentNode ))))); 1)
							else (*Gray*)
								let
									val newG = calculateG(currentNode, (adjX, adjY))
								in
									if newG >= adjG then
										0
									else ((Graph.update grid) ((adjX, adjY), (SOME (Graph.Node(xy, adjl, (Gray,newG,adjH, currentNode ))))); 2)
								end
						end

					val (_, adjList) = case ((Graph.at grid) (x, y)) of (SOME (Graph.Node(tXY, tadjList, (_, g, h, parent)))) => ((Graph.update grid) ((x, y), (SOME (Graph.Node(tXY, tadjList, (Black, g, h, parent))))),tadjList)
				in
					if currentNode = goal then () else
					pathfind' (foldr(fn ((adjX,adjY), xs) => case doStuff(adjX, adjY) of
						(0) => xs
						| (1) => BinoHeap.insert(xs, getF(adjX,adjY), (adjX,adjY))
						| (2) => BinoHeap.update(xs, getF(adjX,adjY), (adjX,adjY))
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

	fun aStarGraph grid = Graph.make(grid, (White, 0,0,(0,0)));

	fun dijkstraGraph grid = Graph.make(grid, NONE : int option);

	fun dijkstra ( grid', start as (sx,sy) : (int * int), goal as (ex, ey)) =
	let
		val grid = Graph.copy(grid');
		val openList = Queue.enqueue (Queue.empty, start)
		val _ = case (Graph.at grid) (sx, sy) of (SOME (Graph.Node(_, adjList, _))) => (Graph.update grid) ((sx, sy), SOME (Graph.Node((start, adjList, SOME 0))))


		fun	dijkstra' openList =
			let
				val (currentNode as (x,y), openList) = if Queue.isEmpty openList then raise Fail "Path not found" else (Queue.head(openList), Queue.dequeue(openList))
				val value = case (Graph.at grid) (x, y) of (SOME (Graph.Node(_, _, SOME value))) => value

				fun doStuff((adjX, adjY), openList) =
					let
						val (adjV, adjAL) = case (Graph.at grid) (adjX, adjY) of (SOME (Graph.Node(_, adjAL, adjV))) => (adjV, adjAL)
					in
						if adjV = NONE then
							if (Int.abs(x - adjX) + Int.abs(y - adjY)) = 2 then
								((Graph.update grid) ((adjX, adjY), (SOME (Graph.Node((adjX, adjY), adjAL, SOME (value+14))))); Queue.enqueue(openList, (adjX, adjY)))
							else
								((Graph.update grid) ((adjX, adjY), (SOME (Graph.Node((adjX, adjY), adjAL, SOME (value+10))))); Queue.enqueue(openList, (adjX, adjY)))
						else
							openList
					end

				val adjList = case (Graph.at grid) (x, y) of (SOME (Graph.Node(_, adjList, _))) => adjList
			in
				if currentNode = goal then openList else
				dijkstra' (foldr doStuff openList adjList)
			end

		fun rewind (pos as (x,y)) =
			let
				val (value, adjXY, adjList) = case (Graph.at grid) (x, y) of (SOME (Graph.Node(_, adjXY::adjList, SOME value))) => (value, adjXY, adjList)

				fun compareV ((x, y), (x2,y2) ) = case ((Graph.at grid) (x, y), (Graph.at grid) (x2, y2)) of
												  ((SOME (Graph.Node(_, _, SOME val1))), (SOME (Graph.Node(_, _, SOME val2)))) => if val1 < val2 then (x,y) else (x2,y2)
												  | ((SOME (Graph.Node(_, _, NONE))), (SOME (Graph.Node(_, _, val2)))) => (x2, y2)
												  | ((SOME (Graph.Node(_, _, val1))), (SOME (Graph.Node(_, _, NONE)))) => (x, y)

			in
				if value = 0 then [] else pos::rewind( (foldr compareV adjXY adjList))
			end
	in
		(dijkstra'(openList); rev (rewind goal))
	end

end
