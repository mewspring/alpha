use "graph.sml";
use "pqueue.sml";
structure Pathfinder =
struct
	datatype Color = White | Gray | Black
	fun aStar ( grid', start as (sx,sy) : (int * int), goal as (ex, ey))  =
		let
			val grid = Graph.copy(grid');
			val openList = BinoHeap.insert( BinoHeap.empty, 0,start );
			fun getParent(x,y) = case Graph.at(grid, x, y) of (SOME (Graph.Node(_, _, (_,_,_, parent )))) => parent
			fun pathfind'  ([]) = raise Fail "Path not found"
			|	pathfind' ( openList ) =
				let
					val ((currentF, (currentNode as (x,y))), openList ) = BinoHeap.extractMin( openList );
					fun calculateG ((x, y), (adjX, adjY)) = case (Graph.at(grid, x, y)) of (SOME (Graph.Node(_, _, (_, g, _, _)))) => if (Int.abs(x - adjX) + Int.abs(y - adjY)) = 2 then g+14 else g+10
					fun calculateH(x,y) = 10*(Int.abs(x - ex) + Int.abs(y - ey))

					fun getF(adjX, adjY) = case Graph.at(grid, adjX, adjY) of (SOME (Graph.Node(_, _, (_,g,h, _ )))) => g+h

					fun hasBestG((currentX, currentY), (adjX, adjY)) = case (Graph.at(grid, currentX, currentY), Graph.at(grid, adjX, adjY)) of ((SOME (Graph.Node(_, _, (_, g, _, _)))), (SOME (Graph.Node(_, _, (_, adjG, _, _))))) => g <= adjG

					fun doStuff (adjX, adjY) =
						let
							val (xy, adjl, adjC,adjG,adjH,adjParent) = case Graph.at(grid, adjX, adjY) of (SOME (Graph.Node(xy, adjl, (c,g,h, parent )))) => (xy, adjl, c,g,h,parent)
						in
							if adjC = Black then
								0
							else if adjC = White then
								(Graph.update(grid, adjX, adjY, (SOME (Graph.Node(xy, adjl, (Gray,calculateG(currentNode, (adjX, adjY)),calculateH(adjX, adjY), currentNode ))))); 1)
							else (*Gray*)
								let
									val newG = calculateG(currentNode, (adjX, adjY))
								in
									if newG >= adjG then
										0
									else (Graph.update(grid, adjX, adjY, (SOME (Graph.Node(xy, adjl, (Gray,newG,adjH, currentNode ))))); 2)
								end
						end

					val (_, adjList) = case Graph.at(grid, x, y) of (SOME (Graph.Node(tXY, tadjList, (_, g, h, parent)))) => (Graph.update(grid, x, y, (SOME (Graph.Node(tXY, tadjList, (Black, g, h, parent))))),tadjList)
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

	fun aStarGrid( grid ) = Graph.makeGraph(grid, (White, 0,0,(0,0)));

	fun dijkstraGrid( grid ) = Graph.makeGraph(grid, 0);


	fun dijkstra ( grid, start as (sx,sy) : (int * int), goal as (ex, ey)) =
	let
		val openList = [start] (*should be a queue*)
		val _ = case Graph.at(grid, sx, sy) of (SOME (Graph.Node(_, adjList, _))) => Graph.update(grid, sx, sy, SOME (Graph.Node((start, adjList, 1))))


		fun dijkstra' [] = raise Fail "Path not found"
		|	dijkstra'((currentNode as (x,y))::openList) =
			let
				val value = case Graph.at(grid, x, y) of (SOME (Graph.Node(_, _, value))) => value

				fun doStuff((adjX, adjY), openList) =
					let
						val (adjV, adjAL) = case Graph.at(grid, adjX, adjY) of (SOME (Graph.Node(_, adjAL, value))) => (value, adjAL)
					in
						if adjV = 0 then
							(Graph.update(grid, adjX, adjY, (SOME (Graph.Node((adjX, adjY), adjAL, value+1)))); openList@[(adjX, adjY)])
						else
							openList
					end

				val adjList = case Graph.at(grid, x, y) of (SOME (Graph.Node(_, adjList, _))) => adjList
			in
				if currentNode = goal then openList else
				dijkstra' (foldr doStuff openList adjList)
			end

		fun rewind (pos as (x,y)) =
			let
				val (value, adjList) = case Graph.at(grid, x, y) of (SOME (Graph.Node(_, adjList, value))) => (value, adjList)

				fun findNextPos((adjX, adjY)::adjList) = case Graph.at(grid, adjX, adjY) of (SOME (Graph.Node(_, _, adjV))) => if adjV = (value-1) then (adjX, adjY) else findNextPos(adjList)
			in
				if value = 1 then [] else pos::rewind( findNextPos(adjList) )
			end

	in
		(dijkstra'(openList); rev (rewind goal))
	end

end
