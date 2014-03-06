structure Vector2 =
struct

	abstype 'a Vector2 = Vec2 of 'a vector vector
	with

		(*
			sub (vec2, x, y)
			TYPE: 'a Vector2 * int * int -> 'a
			PRE: x > 0, y > 0, x < nRows, y > y < nCols
			POST: the element located at row x, col y in vec2.
			EXCEPTIONS: Subscript is raised if x or y < 0 or x >= nRows or y >= nCols
		*)
		fun sub(Vec2 vec2, x, y) = Vector.sub(Vector.sub(vec2, x), y)



		(*
			update (vec2, x, y, r)
			TYPE: 'a Vector2 * int * int * 'a -> 'a Vector2
			PRE: x > 0, y > 0, x < nRows, y > y < nCols
			POST: vec2 with the element at row x, col y replaced by r.
			EXCEPTIONS: Subscript is raised if x or y < 0 or x >= nRows or y >= nCols
		*)
		fun update(Vec2 vec2, x, y, r) =
			if x < 0 orelse y < 0 orelse x >= Vector.length vec2 orelse y >= Vector.length(Vector.sub(vec2, 0)) then
				raise Subscript
			else
				Vec2 (Vector.mapi (fn (j,s) => if j = x then Vector.update(s, y, r) else s) vec2)

		(*
			nRows vec2
			TYPE: 'a Vector2 -> int
			PRE: true
			POST: The number of rows in vec2.
		*)
		fun nRows (Vec2 vec2) = Vector.length vec2

		(*
			nCols vec2
			TYPE: 'a Vector2 -> int
			PRE: vec2 must contain at least one column.
			POST: The number of columns in vec2.
			EXCEPTIONS: Subscript is raised if vec2 does not contain any column(s).
		*)
		fun nCols (Vec2 vec2) = Vector.length(Vector.sub(vec2, 0))

		(*
			appi f vec2
			TYPE: (int * int * 'a -> unit) -> 'a Vector2 -> unit
			PRE: vec2 must contain at least one column.
			POST: none.
			SIDE-EFFECTS: f applied to all elements of vec2 with both the coordinates and element supplied.
			EXCEPTIONS: Subscript is raised if vec2 does not contain any column(s).
		*)
		fun appi (f : int * int * 'a -> unit) (Vec2 vec2) =
			let
				val nrows = Vector.length vec2
				val ncols = Vector.length(Vector.sub(vec2, 0))

				fun appi'(r,c) =
					if r = nrows then ()
					else if c = ncols then appi'(r+1, 0)
					else
						(
							f(r,c, sub(Vec2 vec2, r, c));
							appi'(r, c+1)
						)
			in
				appi'(0,0)
			end


		(*
			modifyi f vec2
			TYPE: (int * int * 'a -> 'a) -> 'a Vector2 -> 'a Vector2
			PRE: vec2 must contain at least one column.
			POST: vec2 with f applied on all of its elements.
			EXCEPTIONS: Subscript is raised if vec2 does not contain any column(s).
		*)
		fun modifyi (f: (int * int * 'a -> 'a)) (Vec2 vec2) =
			let

				val nrows = Vector.length vec2
				val ncols = Vector.length(Vector.sub(vec2, 0))

				fun modifyi''(r,c) =
					if c = ncols then
						[]
					else
						f(r,c, sub(Vec2 vec2, r, c))::modifyi''(r, c+1)

				fun modifyi'(r) =
					if r = nrows then
						[]
					else
						(Vector.fromList (modifyi''(r,0)))::modifyi'(r+1)


			in
				Vec2(Vector.fromList (modifyi'(0)))
			end

		(*
			vector (r,c, init)
			TYPE: int * int * 'a -> 'a Vector2
			PRE: r and c have to be non-negative.
			POST: A Vector2 with r rows and c columns, with every element assigned init.
		*)
		fun vector (r, c, init) = Vec2(Vector.fromList (List.tabulate(r, fn _ => Vector.tabulate(c, fn _ => init ))))

		(*
			fromList l
			TYPE: 'a list list -> 'a Vector2
			PRE: Every list in l must contain the same amount of elements.
			POST: Vector2 created from l.
			EXCEPTIONS: Size raised if all lists in l are not uniform in size.
		*)
		fun fromList l =
			 let
				fun checkLen(l, NONE) = SOME(List.length l)
				| checkLen(l, SOME i) =
				if List.length l <> i then
					raise Size
				else
					SOME i
				val _ = List.foldl checkLen NONE l
			in
				Vec2(Vector.fromList(List.map (fn ll => Vector.fromList ll) l))
			end
	end
end