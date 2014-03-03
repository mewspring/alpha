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

	(*
		loopExtend ( tilegrid, h, v )
		TYPE: tilegrid * int * int -> tilegrid
		PRE: v and h >= 0
		POST: tilegrid extended vertically by v rows and horizontally
			 by h columns by repeating from the first row and column
		EXAMPLE:
		loopExtend( [[1,2,3,4],[1,2,3,4],[1,2,3,4]], 2, 3 ) =
			 [[1,2,3,4,1,2,3],[1,2,3,4,1,2,3],[1,2,3,4,1,2,3],[1,2,3,4,1,2,3],[1,2,3,4,1,2,3]]
	*)
	fun loopExtend( tilegrid, h, v ) =
		let
			fun loopExtend( l, 0 ) = l
			|	loopExtend( l, n ) =
				let
					val lenl = (List.length l)
					val remainder = n mod lenl
				in
					if n > lenl then
						loopExtend( l@l, n-lenl )
					else
						l@List.take(l,n)
				end
		in
			loopExtend( (map (fn x=> loopExtend(x,h)) tilegrid), v )
		end
end;
