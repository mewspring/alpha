use "grid.sml";
use "vector2.sml";

structure Graph =
struct
	(*
		DATATYPE REPRESENTATION:
		   a graph node with the coordinate (x, y), an adjacent list containing
		   the coordinates of each adjacent walkable node and any additional node
		   data is represented by:
		      Node((x, y), adjacent, data)
		DATATYPE CONVENSION:
		   the adjacent list should contain the coordinates of each walkable
		   adjacent node of the node. If there are no adjacent walkable nodes the
		   adjacent list is empty.
	*)
	datatype 'a node = Node of (int * int) * (int * int) list * 'a;

	(*
		getNode grid ((x, y), data)
		TYPE: Grid.grid -> int * int * 'a -> node option
		PRE: true
		POST: SOME node at the provided (x, y) coordinate with its associated
		      adjacent nodes and node data, or NONE if the specified coordinate
		      isn't walkable.
	*)

	fun getNode grid ((x, y), data) =
		if Grid.canWalk grid (x, y) then
			SOME (Node((x, y), Grid.getAdjacent grid (x, y), data))
		else
			NONE;

	abstype 'a graph = Graph of 'a node option Vector2.Vector2
	with
		(*
			make (grid, data)
			TYPE: grid * 'a -> a' graph
			PRE: true
			POST: the graph representation of the provided grid, where each cell of
			      the grid corresponds to a node in the graph; and where all nodes of
			      the graph are connected with their adjecent nodes in the grid.
			      ref: getNode
		 *)
		fun make (grid, data) =
			let
				val graph = Vector2.vector(Grid.height grid, Grid.width grid, NONE)
				fun f (y, x, _) = getNode grid ((x, y), data)
				val graph = Vector2.modifyi f graph
			in
				Graph graph
			end;

		(*
			at graph (x, y)
			TYPE: graph -> int * int -> node
			PRE: (x, y) is a valid coordinate of the graph.
			POST: the graph node located at the coordinate (x, y).
		*)
		fun at (Graph graph) (x, y) = Vector2.sub(graph, y, x);

		(*
			update graph ((x, y), data)
			TYPE: graph -> (int * int) * 'a -> unit
			PRE: (x, y) is a valid coordinate of the graph.
			POST: none.
			SIDE-EFFECTS: updates the data of the graph node at the provided (x, y)
			              coordinate.
		*)
		fun update (Graph graph) ((x, y), data) = Graph (Vector2.update(graph, y, x, data));
	end;
end;
