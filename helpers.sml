structure Helpers =
	struct
	(*
		readLines path
		TYPE: string -> string list
		PRE: path must be the path of an existing (readable) file.
		POST: a list of lines that compromise the file at path.
	*)
	fun readLines path =
		let
			(*
				readLines' s
				TYPE: TextIO.instream -> string list
				PRE: s is an open instream.
				POST: a list of all lines read from s until its empty.
			*)
			fun readLines' s =
				case TextIO.inputLine s of
				    NONE => (TextIO.closeIn s; [])
				  | SOME(line) => line::readLines' s
		in
			readLines' (TextIO.openIn path)
		end;

	(*
		getIntList s
		TYPE: string -> int list
		PRE: true
		POST: a list of integers based on the comma separated numbers in s.
		EXAMPLE: getIntList "1,2,3" = [1,2,3]
		         getIntList "10,foo,20" = [10,20]
	*)
	fun getIntList s =
		let
			(*
				getInt s
				TYPE: substring -> int option
				PRE: true
				POST: the integer representation of s if valid, or NONE otherwise.
				EXAMPLE: getInt (Substring.full("1")) = SOME 1
				         getInt (Substring.full("foo")) = NONE
			*)
			fun getInt s = Int.fromString(Substring.string(s))
			(*
				split (s, c)
				TYPE: string * char -> substring list
				PRE: true
				POST: s split into all substrings separated by c.
				EXAMPLE: split ("foo,bar", #","); val it = ["foo", "bar"]: substring list
			*)
			fun split (s, c) = Substring.fields (fn x => x = c) (Substring.full s)
		in
			List.mapPartial getInt (split (s, #","))
		end;

	(*
		contains (l, x)
		TYPE: 'a list * 'a -> bool
		PRE: true
		POST: true if the list l contains x and false otherwise.
		EXAMPLE: contains ([1,2,3], 3) = true
		         contains ([1,2,3], 4) = false
	*)
	fun contains (l, x) =
		List.exists (fn v => v = x) l;

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

end
