use "graph.sml";
use "pqueue.sml";

structure AStar =
struct
	datatype color = White | Gray | Black;

	(*
		makeGraph grid
		TYPE: Grid.grid -> (color * int * int * (int * int)) Graph.graph
		PRE: true
		POST: a graph where each node contains data relevant to the A* algorithm,
		      which corresponds to the color of the node, its G, H costs and the
		      coordinate of its parent respectively.
	*)
	fun makeGraph grid =
		let
			val G = 0
			val H = 0
			val ppos = (0, 0)
			val data = (White, G, H, ppos)
		in
			Graph.make(grid, data)
		end;

	(*
		find (graph, spos, epos)
		TYPE: (color * int * int * (int * int)) Graph.graph * (int * int) * (int * int) -> (int * int) list option
		PRE: spos and epos are valid coordinates of the graph.
		POST: a path represented as a list of coordinates, which corresponds to
		      the steps taken to reach the end node at epos from the start node at
		      spos, or NONE if no such path exists.
	*)
	fun find (graph, spos as (sx, sy), epos as (ex, ey)) =
		let
			val graph' = Graph.copy graph
			val start = (Graph.at graph') spos

			(*
				h (x, y)
				TYPE: int * int -> int
				PRE: (x, y) is a valid coordinate of the graph.
				POST: the estimated distance from the provided (x, y) coordinate to
				      the end node at epos, using the Manhattan distance [1] as a
				      heuristic measurement.
				      [1]: http://mathworld.wolfram.com/TaxicabMetric.html
			*)
			fun h (x, y) = Int.abs(ex - x) + Int.abs(ey - y)

			(* Insert the start node. *)
			val openList = Pqueue.insert(Pqueue.empty, 0 + h spos, valOf start)
			(*
				find' ol
				TYPE: (color * int * int * (int * int)) Pqueue.queue -> (int * int) list option
				PRE: true
				POST: a path represented as a list of coordinates, which corresponds
				      to the steps taken to reach the end node at epos from the
				      start node at spos, or NONE if no such path exists. The nodes
				      to process while locating this path are taken one at the time
				      from the open list ol. For each node processed this way its
				      adjacent nodes which haven't been processed already are added
				      to ol for future processing.
			*)

			fun find' ol =
				if Pqueue.isEmpty ol then
					(* Unable to located a path from spos to epos. *)
					NONE
				else
					let
						val ((_, node as (Graph.Node(pos as (x, y), adjacent, (color, G, H, ppos)))), ol) = Pqueue.extractMin(ol)

						(*
							edgeCost apos as (ax, ay)
							TYPE: int * int -> int
							PRE: apos is a valid coordinate of the graph and it is
							     distinct from the (x, y) coordinate. It should be
							     within 1 units x- and y-distance from the current node
							     at the (x, y) coordinate.
							POST: the cost of traversing the edge from the current node
							      at (x, y) to the adjacent node at apos.
							SIDE-EFFECTS: raises Fail if apos isn't distinct from the
							              (x, y) coordinate or if apos is further away
							              than 1 unit in x- or y-distance from the
							              current node at the (x, y) coordinate.
						*)
						fun edgeCost (ax, ay) =
							let
								val dist = Int.abs(ax - x) + Int.abs(ay - y)
							in
								if dist = 0 then
									raise Fail "invalid apos; the coordinate of the current node and one of its adjacent node are not distinct from one another."
								else if dist = 1 then
									(* The edge cost of a vertical or horizontal step
									   times 10, i.e. 1*10 *)
									10
								else if dist = 2 then
									(* The edge cost of a diagonal step times 10, i.e.
									   approximately sqrt(1+1)*10. *)
									14
								else
									raise Fail "invalid apos; the adjacent coordinate is more than 1 unit in x- or y-distance away from the coordinate of the current node."
							end

						(*
							process (ol, anode as Graph.Node(apos, _, _))
							TYPE: (color * int * int * (int * int)) Pqueue.queue * (color * int * int * (int * int)) Graph.node -> (color * int * int * (int * int)) Pqueue.queue
							PRE: true
							POST: a priority queue with anode added if not present in
							      the queue since before, or updated if a shorter path
							      to apos has been located through the parent node at
							      pos.
							SIDE-EFFECTS: if the path from spos to apos is shorter
							              using the parent node at pos then update the
							              anode's parent and G cost.
						*)
						fun process (ol, anode as Graph.Node(apos as (ax, ay), aAdjacent, (aColor, aG, aH, _))) =
							let
								val newG = G + edgeCost apos
								val aF = newG + aH
							in
								if aColor = White then
									let
										(* pos is the coordinate of the parent node. *)
										val adata = (Gray, newG, h apos, pos)
										val newAnode = Graph.Node(apos, aAdjacent, adata)
									in
										(* A path to anode has been located. Update its
										   data in the graph and add it to the queue. *)
										(
											(Graph.update graph') (apos, SOME newAnode);
											Pqueue.insert(ol, aF, newAnode)
										)
									end
								else if aColor = Gray andalso newG < aG then
									let
										(* pos is the coordinate of the parent node. *)
										val adata = (aColor, newG, aH, pos)
										val newAnode = Graph.Node(apos, aAdjacent, adata)
									in
										(* A shorter path to anode has been located.
										   Update its data in the graph and in the queue.
										*)
										(
											(Graph.update graph') (apos, SOME newAnode);
											Pqueue.update(ol, aF, newAnode)
										)
									end
								else (* color = Black *)
									(* The node has already been processed and no shorter
									   path was located. *)
									ol
							end

						(*
							backtrack node as (Graph.Node(pos, _, (_, _, _, ppos)))
							TYPE: (color * int * int * (int * int)) Graph.node -> int list
							PRE: node is SOME valid node of the graph and there is a
							     path from node to snode connected through the parent
							     of each node.
							POST: a list of coordinates which represents the path from
							      pos to spos through the parent coordinate ppos of
							      each intermediate node.
						*)
						fun backtrack (node as (Graph.Node(pos, _, (_, _, _, ppos)))) =
							if pos = spos then
								[]
							else
								pos::backtrack (valOf ((Graph.at graph') ppos))

						(*
							processAdjacent (ol, adjacent)
							TYPE: (color * int * int * (int * int)) Pqueue.queue * (int * int) list -> (color * int * int * (int * int)) Pqueue.queue
							PRE: for each pos in adjacent: pos is a valid coordinate of
							     the grid.
							POST: a priority queue with each adjacent node added to it,
							      after processing said node.
							SIDE-EFFECTS: process each node in the adjacent list. ref:
							              SIDE-EFFECTS of process.
						*)
						fun processAdjacent (ol, []) = ol
						  | processAdjacent (ol, apos::adjacent) =
							processAdjacent(process(ol, valOf ((Graph.at graph') apos)), adjacent)

						(*
							markVisit ()
							TYPE: unit -> unit
							PRE: true
							POST: none
							SIDE-EFFECTS: marks the current node as visited by updating
							              its color to Black in the graph.
						*)
						fun markVisit () =
							(Graph.update graph') (pos, SOME (Graph.Node(pos, adjacent, (Black, G, H, ppos))))

					in
						if pos = epos then
							(* A path has been found. Backtrack to the end node to the
							   start node through the parent of each node. Since we are
							   backtracking the list will be in reverse order. *)
							SOME (rev (backtrack node))
						else
							(markVisit(); find' (processAdjacent (ol, adjacent)))
					end
		in
			find' openList
		end;
end;
