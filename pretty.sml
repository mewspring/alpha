use "helpers.sml";
use "grid.sml";

structure Pretty =
struct

	fun printGrid grid =
		let
			val grid' = Grid.toArray2 grid
			val region = {base=grid', row=0, col=0, nrows=NONE, ncols=NONE}
			fun f (x, y, a) =
				(if a then print(".") else print("#");
				if y = Grid.width grid - 1 then print("\n") else ())
		in
			Array2.appi Array2.RowMajor f region
		end;

	fun printPath (grid, start, goal, path) =
		let
			val region = {base=grid, row=0, col=0, nrows=NONE, ncols=NONE}
			fun f (x, y, a) =
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
				if y = Array2.nCols(grid)-1 then
					print("\n")
				else
					())
		in
			Array2.appi Array2.RowMajor f region
		end;

end;
