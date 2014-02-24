(*
--------------------------------------------------------------------------------

=== [ Graph ] ===
*)
datatype Color = White | Gray | Black

(*
	Node struct {
		(x, y)
		// List of adjacent nodes.
		(x, y) list
		ref Color
	}
*)
datatype node = Node of (int * int) * (int * int) list * (Color ref) 

abstype graph = Graph of node option Array2.array 
with

	fun at(Graph g, (x, y))  = Array2.sub(g, x,y)  

	fun new mb2 = 
		let
			val map = Array2.array(Array2.nRows mb2, Array2.nCols mb2, NONE) : node option Array2.array
		
		fun findAdjacent(i, j) = 
			let
				val adjacentNodes = [(i-1, j-1), (i-1, j),(i-1, j+1),(i, j-1),(i, j+1),(i+1, j-1),(i+1, j),(i+1, j+1)]
				fun isInBound(i, j) = i >= 0 andalso i < Array2.nRows mb2 andalso j >= 0 andalso j < Array2.nCols mb2					
			in				
				foldr (fn ((i,j), xs) => if isInBound (i,j) andalso Array2.sub(mb2, i, j) then (i,j)::xs else xs) [] adjacentNodes				
			end
		
		fun iterate'(arr2, i, j) = if j >= Array2.nCols arr2 then () else if Array2.sub(mb2, i, j) then (Array2.update(arr2, i, j, (SOME (Node((i,j), findAdjacent(i,j), (ref White))))); iterate'(arr2, i, j+1)) else iterate'(arr2, i, j+1) 
		fun iterate (arr2, i) 	 = if i >= Array2.nRows arr2 then () else (iterate'(arr2, i, 0); iterate(arr2,i+1)) 		
		in
			Graph (iterate(map, 0); map)
		end
end
(*
=== [ Pathfinder ] ===

path = find(g Graph, start, goal) (x, y) list option
*)