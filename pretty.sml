use "grid.sml";
use "helpers.sml";
use "vector2.sml";

structure Pretty =
struct
	(*
		printGrid grid
		TYPE: bool Vector2.vector -> unit
		PRE: true
		POST: none
		SIDE-EFFECTS: pretty prints the grid to standard output.
	*)
	fun printGrid grid =
		let
			fun f (y, x, a) =
				(if a then print(".") else print("#");
				if x = Vector2.nCols grid - 1 then print("\n") else ())
		in
			Vector2.appi f grid
		end;

	(*
		printPath (grid, start, goal, path)
		TYPE: bool Vector2.vector * (int * int)
		PRE: true
		POST: none
		SIDE-EFFECTS: pretty prints the path from the start to the goal of the
		              grid to standard output.
	*)
	fun printPath (grid, start, goal, path) =
		let
			fun f (y, x, a) =
				(if (x, y) = start then
					print("S")
				else if (x, y) = goal then
					print("E")
				else if Helpers.contains (path, (x, y)) then
					print("x")
				else if a then
					print(".")
				else
					print("#");
				if x = Vector2.nCols grid - 1 then
					print("\n")
				else
					())
		in
			Vector2.appi f grid
		end;
end;
