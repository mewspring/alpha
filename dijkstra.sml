use "astar.sml";

structure Dijkstra =
struct
	val makeGraph = AStar.makeGraph

	(*
		h epos pos
		TYPE: (int * int) -> (int * int) -> unit
		PRE: true
		POST: Always 0. The Dijkstra algorithm is basically equivalent to the A*
		      algorithm without a heuristic function. Therefore h is provided as a
		      simple nop (No Operation) heuristic function.
	*)
	fun h _ _ = 0;

	val find = AStar.find h;
end;
