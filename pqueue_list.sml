structure Pqueue = 
	struct
	(*  REPRESENTATION CONVENTION:
	    A list of elements with key d and data d

	    REPRESENTATION INVARIANT:
	    - the key of each element is less or 
	      equal to the key of the next one
	    - the data of each element must be unique
	*)
	type ''a queue = (int * ''a) list

	(* empty
	   TYPE: queue
	   PRE: (none)
	   POST: an empty queue
	*)
	val empty = []

	(* 	isEmpty T
		TYPE: queue -> bool
		PRE:  (none)
		POST: true if and only if the queue is empty
	*)
	fun isEmpty [] = true
	|	isEmpty _  = false 

	(*	insert (Q, k, d )
		TYPE: queue * int * ''a -> queue
		PRE:  (none)
		POST: Q with element of k and d
	*)
	fun insert ([], k, d) = [(k, d)]
	|	insert ( (x as (k1,d1))::xs, k, d) = if k <= k1 then (k, d)::(x::xs) else x::insert(xs, k, d)

	(* 	extractMin Q
		TYPE: queue -> (int,''a) * queue
		PRE:  Q is not empty
		POST: ((k,d), Q’), where k is the minimum key of Q and d is the data of that element,
		      and H’ is H without that element
	*)
	fun extractMin queue = (hd queue, tl queue)

	(*	remove (Q, d )
		TYPE: queue * ''a -> queue
		PRE:  (none)
		POST: Q without element with d
	*)
	fun remove( [], d ) = []
	|	remove( (x as (_,d1))::xs, d) = if d1 = d then xs else x::remove(xs,d)

	(* 	update( Q, k, d )
		TYPE: queue * int * ''a -> queue
		PRE: (none)
		POST: Q with updated key to k for element which has data = d
	*)
	fun update( queue, k, d ) = insert( remove( queue, d ), k, d )
end