use "helpers.sml";

structure TileGrid =
struct
	type tilegrid = int list list;

	(*
		make (tilesPath)
		TYPE: string -> tilegrid
		PRE: the provided file contains comma-separated integers. 
		POST: a tilegrid as specified tilesPath.
	*)
	fun make (tilesPath) : tilegrid =
			List.map Helpers.getIntList (Helpers.readLines tilesPath)

	(*
		merge ( bottomLayer, topLayer)
		TYPE: tilegrid * tilegrid -> tilegrid
		PRE: bottomLayer has the same dimensions as topLayer
		POST: a grid which is the result of merging two layers such that
			 cells will have the value of topLayer unless that value is 0,
			  then it will have the value of bottomLayer
	*)
	fun merge(bottomLayer, topLayer) : tilegrid =
		ListPair.map (fn (fl, ol) => ListPair.map (fn (f, obj) => if obj <> 0 then obj else f) (fl, ol)) (bottomLayer, topLayer)
end;
