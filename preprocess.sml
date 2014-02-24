PolyML.print_depth 100;

use "helpers.sml";

(*
	readGrid (floorPath, objectsPath)
	TYPE: string * string -> int list list
	PRE:
	POST:
*)
fun readGrid (floorPath, objectsPath) =
	let
		val floor = List.map getIntList (readLines floorPath)
		val objects = List.map getIntList (readLines objectsPath)
	in
		(* one liner magic :) *)
		ListPair.map (fn (fl, ol) => ListPair.map (fn (f, obj) => if obj <> 0 then obj else f) (fl, ol)) (floor, objects)
	end;

(*
	readWalkable walkPath
	TYPE: string -> int list
	PRE:
	POST:
*)
fun readWalkable walkPath =
	getIntList(hd (readLines walkPath));

val grid = readGrid("floor.txt", "objects.txt");
val walkable = readWalkable "walk.txt";

(*
	preprocess (grid, walkable)
	TYPE: int list list, int list -> bool Array2.array
	PRE: each element in the two dimensional grid corresponds to a tile ID and
	     the walkable tile IDs are present in the walkable list.
	POST: a two dimensional array where each element is either true (walkable) or
	      false (not walkable).
*)
fun preprocess (grid, walkable) =
	Array2.fromList(List.map (fn row => List.map (fn cell => contains(walkable, cell)) row) grid);

preprocess(grid, walkable);
