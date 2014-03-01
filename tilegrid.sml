use "helpers.sml";

structure TileGrid =
struct
	type tilegrid = int list list;

	(*
		make (floorPath, objectsPath)
		TYPE: string * string -> tilegrid
		PRE: each of the provided files contains comma-separated integers. The
		     floorPath file contains the tile IDs for the floor layer of the map
		     and the objectsPath contains the tile IDs for the objects layer of
		     the map (which is above the floor layer). Both of these files
		     contains the same number of lines, with comma-separated tile IDs, as
		     there are rows in the grid.
		POST: a grid which is the result of merging the floor layer, as specified
		      by floorPath, with the objects layer, as specified by objectsPath.
	*)
	fun make (floorPath, objectsPath) : tilegrid =
		let
			val floor = List.map Helpers.getIntList (Helpers.readLines floorPath)
			val objects = List.map Helpers.getIntList (Helpers.readLines objectsPath)
		in
			(* one liner magic :) *)
			ListPair.map (fn (fl, ol) => ListPair.map (fn (f, obj) => if obj <> 0 then obj else f) (fl, ol)) (floor, objects)
		end;
end;
