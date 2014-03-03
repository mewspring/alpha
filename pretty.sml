use "grid.sml";
use "helpers.sml";

structure Pretty =
struct
	(*
		printGrid grid
		TYPE: bool Array2.array -> unit
		PRE: true
		POST: none
		SIDE-EFFECTS: pretty prints the grid to standard output.
	*)
	fun printGrid grid =
		let
			val region = {base=grid, row=0, col=0, nrows=NONE, ncols=NONE}
			fun f (y, x, a) =
				(if a then print(".") else print("#");
				if x = Array2.nCols grid - 1 then print("\n") else ())
		in
			Array2.appi Array2.RowMajor f region
		end;

	(*
		printPath (grid, start, goal, path)
		TYPE: bool Array2.array * (int * int)
		PRE: true
		POST: none
		SIDE-EFFECTS: pretty prints the path from the start to the goal of the
		              grid to standard output.
	*)
	fun printPath (grid, start, goal, path) =
		let
			val region = {base=grid, row=0, col=0, nrows=NONE, ncols=NONE}
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
				if x = Array2.nCols grid - 1 then
					print("\n")
				else
					())
		in
			Array2.appi Array2.RowMajor f region
		end;
end;
