structure Queue =
struct
	(*

	REPRESENTATION CONVENTION: the term
	  Queue([x1,x2,...,xn],[y1,y2,...,ym]) represents the
	  queue x1,x2,...,xn,ym,...,y2,y1 with head x1 and tail y1

	REPRESENTATION INVARIANT: (see next slide)
	*)
	type 'a queue = 'a list * 'a list


	(*
	 empty
	 TYPE:  ’a queue
	 VALUE: the empty queue
	*)
	val empty = ([],[])

	(*
	 isEmpty Q
	 TYPE: ’a queue -> bool
	 PRE:  (none)
	 POST: true if Q is empty, and false otherwise
	*)
	fun isEmpty ([],[]) = true
	  | isEmpty _ = false

	(*
	 head Q
	 TYPE: ’a queue -> ’a
	 PRE:  Q is nonempty
	 POST: the head element of Q
	*)
	fun head ((x :: _), _) = x

	fun normalize ([], ys) = (rev ys, [])
	  | normalize q = q

	(*
	dequeue Q
	TYPE: ’a queue -> ’a queue
	PRE:  Q is nonempty
	POST: Q without its head element
	*)
	fun dequeue ([x], ys) = normalize ([], ys)
	  | dequeue ((x :: xs), ys) = (xs, ys)

	(*
	enqueue (Q, t)
	TYPE: ’a queue * ’a -> ’a queue
	PRE:  (none)
	POST: the queue Q with t added as new tail element
	*)
	fun enqueue ((xs, ys), e) = normalize (xs, e :: ys)

	(*
	toList Q
	TYPE: ’a queue -> ’a list
	PRE:  (none)
	POST: the representation of Q in list form, with the head of Q as
	head, and so on
	*)
	fun toList (xs, ys) = xs @ rev ys
end