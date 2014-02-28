use "helpers.sml";

structure Grid =
struct
	fun make (floorPath, objectsPath, walkablePath) =
	let
		val grid' = Helpers.readGrid(floorPath, objectsPath)
		val walkable = Helpers.readWalkable walkablePath
	in
		Helpers.preprocess(grid', walkable)
	end;
end;
