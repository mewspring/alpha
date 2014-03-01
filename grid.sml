use "helpers.sml";
use "tilegrid.sml";

structure Grid =
struct
	(*
		readWalkable walkPath
		TYPE: string -> int list
		PRE: The walkPath file contains a comma-separated list of walkable tile
		     IDs.
		POST: a list of walkable tile IDs.
	*)
	fun readWalkable walkPath =
		Helpers.getIntList(hd (Helpers.readLines walkPath));

	(*
		DATATYPE REPRESENTATION:
		   a grid divides an area into a series of contiguous grid cells using
		   regular tessellation. The underlying datatype of a grid is a
		   two-dimensional array of boolean values. Each boolean value represents
		   the property of a cell with regards to if it's walkable or not.
		DATATYPE CONVENSION:
		   each cell of a grid is true if walkable, and false otherwise. The width
		   and height of the grid in number of cells in equivalent to the numer of
		   cols and rows in the underlying two-dimensional array respectively.
	*)
	abstype grid = Grid of bool Array2.array
	with
		(*
			make (floorPath, objectsPath, walkPath)
			TYPE: string * string * string -> grid
			PRE: each of the provided files contains comma-separated integers. The
			     floorPath file contains the tile IDs for the floor layer of the map
			     and the objectsPath contains the tile IDs for the objects layer of
			     the map (which is above the floor layer). Both of these files
			     contains the same number of lines, with comma-separated tile IDs, as
			     there are rows in the grid. The walkPath file contains a
			     comma-separated list of tileIDs which are walkable; and all other
			     tile IDs are considiered not walkable.
			POST: a grid which is the result of merging the floor layer, as specified
			      by floorPath, with the objects layer, as specified by objectsPath.
			      Each cell of the grid is either true (walkable) or false (not
			      walkable) as specified by walkPath.
		*)
		fun make (floorPath, objectsPath, walkPath) =
			let
				val tilegrid = TileGrid.make(floorPath, objectsPath)
				val walkable = readWalkable walkPath
				(*
					preprocess (tilegrid, walkable)
					TYPE: int list list, int list -> bool Array2.array
					PRE: each element in the two-dimensional tilegrid corresponds to a
					     tile ID and the walkable tile IDs are present in the walkable
					     list.
					POST: a two dimensional array where each element is either true
					      (walkable) or false (not walkable).
				*)
				fun preprocess (tilegrid, walkable) =
					Grid (Array2.fromList(List.map (fn row => List.map (fn cell => Helpers.contains(walkable, cell)) row) tilegrid))
			in
				preprocess(tilegrid, walkable)
			end;

		(*
			width grid
			TYPE: grid -> int
			PRE: true
			POST: returns the width of the grid in number of columns.
		*)
		fun width (Grid grid) =
			Array2.nCols grid;

		(*
			height grid
			TYPE: grid -> int
			PRE: true
			POST: returns the height of the grid in number of rows.
		*)
		fun height (Grid grid) =
			Array2.nRows grid;

		(*
			canWalk grid (x, y)
			TYPE: grid -> int * int -> bool
			PRE: true
			POST: true if the (x, y) coordinate of the grid is walkable, and false
			      otherwise.
		*)
		fun canWalk (Grid grid) (x, y) =
			let
				(*
					isValid (x, y)
					TYPE: (int * int) -> bool
					PRE: true
					POST: true if (x, y) is a valid coordinate of the grid.
				*)
				fun isValid (x, y) =
					x >= 0 andalso x < Array2.nCols(grid) andalso y >= 0 andalso y < Array2.nRows(grid)
			in
				isValid (x, y) andalso Array2.sub(grid, y, x)
			end;

		(*
			getAdjacent grid (x, y)
			TYPE: grid -> int * int -> (int * int) list
			PRE: true
			POST: a list containing the coordinates of adjacent walkable nodes.
		*)
		(* TODO: prevent walks through diagonal walls. *)
		fun getAdjacent grid (x, y) =
			let
				(*
					getWalk (x, y)
					TYPE: int * int -> (int * int) option
					PRE: true
					POST: SOME (x, y) if the provided coordinate of the grid is walkable
					      and NONE otherwise.
				*)
				fun getWalk (x, y) =
					if canWalk grid (x, y) then
						SOME (x, y)
					else
						NONE
			in
				List.mapPartial getWalk [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
			end;

		(*
			toArray2 grid
			TYPE: grid -> bool Array2.array
			PRE: true
			POST: the underlying two-dimensional boolean array of the grid.
		*)
		fun toArray2 (Grid grid) =
			grid;
	end;
end;
