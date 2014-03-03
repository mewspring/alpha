structure Pqueue = 
	struct
	(* BEGIN: From the slides lecture 28 with the addition that each node holds ''a data*)

	
	(*  REPRESENTATION CONVENTION: the first integer, r, is the rank
    of the tree; the second integer k is the key at its root; The third 
    value d can hold any ''a data

    REPRESENTATION INVARIANT: the list has r sub-trees,
    ordered by decreasing ranks r-1, r-2, ..., 1, 0.
	*)
	abstype ''a binoTree = Node of int * int * ''a * ''a binoTree list
	with

		(*  REPRESENTATION CONVENTION:
	    (none: the declaration matches the definition!)

	    REPRESENTATION INVARIANT:
	    - in each binomial tree, the key of each non-root node
	      is at least the key of its parent (min-heap property)
	      (hence the root of each tree contains its minimum key);
	    - the trees have increasing ranks.
	    - the data for each node across all binomial trees must be unique
		*)
		type ''a queue = ''a binoTree list


		(* Some selectors *)

		(* rank T
		   TYPE: binoTree -> int;  
		   PRE:  (none)
		   POST: the rank of T
		*)
		fun rank (Node(r,_,_,_)) = r

		(* root T
		   TYPE: binoTree -> int
		   PRE:  (none)
		   POST: the key of the root of T
		*)
		fun root (Node(_,k,_,_)) = k

		(* empty
		   TYPE: binoHeap
		   PRE: (none)
		   POST: an empty binomial heap
		*)
		val empty = []

		(* 	isEmpty T
			TYPE: binoHeap -> bool
			PRE:  (none)
			POST: true if and only if the binomial heap is empty
		*)
		fun isEmpty [] = true
		  | isEmpty _  = false 

		(* 	link (T1, T2)
			TYPE: binoTree * binoTree -> binoTree
			PRE: rank(T1) = rank(T2)
			POST: the union of the elements of T1 and T2;
			      if T1 and T2 satisfy the min-heap property,
			      then the result must satisfy the min-heap property
		*)
		fun link (T1 as Node(r1,k1,d1,Ts1), T2 as Node(r2,k2,d2,Ts2)) =
			if k1 < k2 then
				Node(r1+1,k1,d1,T2::Ts1)
			else
				Node(r1+1,k2,d2,T1::Ts2)

		(*	insTree (H, T)
			TYPE: binoHeap * binoTree -> binoHeap
			PRE:  T satisfies the min-heap property;
			   rank(T) ≤ rank(T’) for every binomial tree T’ in H 
			POST: the union of the elements of H and T
			VARIANT: |H|
		*)
		fun insTree ([], T) = [T]
		   | insTree (H as T'::H', T) =
			 if rank T < rank T' then T::H
			 else if rank T' < rank T then T' :: insTree (H', T) 
			 else insTree (H', link (T, T'))

		(*	insert (H, k, d )
			TYPE: binoHeap * int * ''a -> binoHeap
			PRE:  (none)
			POST: H with element of k and d
		*)
		fun insert (H, k, d) = insTree (H, Node(0,k,d,[]))

		(*	merge (H1, H2)
			TYPE: binoHeap * binoHeap -> binoHeap
			PRE:  (none)
			POST: the union of H1 and H2
			VARIANT: |H1|·|H2|
		*)
		fun merge (H1, []) = H1 
		  | merge ([], H2) = H2
		  | merge (H1 as T1::H1', H2 as T2::H2') =
			if rank T1 < rank T2 then T1 :: merge (H1', H2)
			else if rank T2 < rank T1 then T2 :: merge (H1, H2')
				 else insTree (merge (H1', H2'), link (T1, T2))


		(*	extractMinTree H
			TYPE: binoHeap -> binoTree * binoHeap
			PRE:  H is not empty
			POST: (T’, H’) where T’ is a binomial tree of H
			      containing an element with the minimum key of H,
			      and H’ is H without T’
			VARIANT: |H|
		*)
		fun extractMinTree [T] = (T, [])
		  | extractMinTree (T::H) =
			let val (T', H') = extractMinTree H
			in  if root T < root T' then (T, H) else (T', T::H') end

		(* 	extractMin H
			TYPE: binoHeap -> (int,''a) * binoHeap
			PRE:  H is not empty
			POST: ((k,d), H’), where k is the minimum key of H and d is the data of the element,
			      and H’ is H without that element
		*)
		fun extractMin H =
			let
				val (Node(_,k,d,H1), H2) = extractMinTree H
			in
				((k,d), merge(rev H1, H2))
			end

		(* END --- From the slides lecture 28 *)

		(* 	update( H, k, d )
			TYPE: binoHeap * int * ''a -> binoHeap
			PRE: element in H which has data = d must have key > k
			POST: H with updated key to k for element which has data = d
			SIDE-EFFECTS: raise Fail if updating with a higher key
		*)
		fun update( Ts, k, d ) = 
			let
				(* 	update'( T, k, d )
					TYPE: binoTree * int * ''a -> binoTree
					PRE: element in T which has data = d must have key > k
					POST: T with updated key to k for element which has data = d
					SIDE-EFFECTS: raise Fail if updating with a higher key
				*)
				fun update'( Node( r, k1, d1, Ts), k, d ) =
					if d1 = d then
						if k < k1 then
							Node( r, k, d1, Ts )
						else
							raise Fail "update'-> Trying to update with a higher or equal key"
					else
						processChildren( Node(r, k1, d1, [] ), Ts, k, d )

				(* 	processChildren( T, Ts, k, d )
				TYPE: binoTree * binoTree list * int * ''a -> binoTree
				PRE: element in Ts which has data = d must have key > k
				POST: T with Ts added as children but with updated key to k for element in Ts
					which has data = d. If key of element in Ts < key of element T then these
					two elements will swap place to maintain the invariant for T
				SIDE-EFFECTS: raise Fail if updating with a higher key
				VARIANT: |Ts|
				*)
				and	processChildren( Node(r1,k1, d1, res), [], k, d ) = Node( r1, k1, d1, rev res )
				|	processChildren( Node(r1,k1, d1, res), T::Ts, k, d ) =
					let
						val Node(r2,k2,d2,Ts2) = update'( T, k, d)
					in
						if k2 < k1 then (* swap *)
							Node( r1, k2, d2, (rev res)@(Node(r2,k1,d1,Ts2)::Ts))
						else
							processChildren( Node( r1, k1, d1, Node(r2,k2,d2,Ts2)::res), Ts, k, d )
					end

			in map (fn x => update'( x, k, d )) Ts end

		(* printHeap H
		TYPE: binoHeap -> ()
		PRE: (none)
		POST: (key,data) of each element in H printed in order with increasing tab-width 
			for each level of the trees
		*)
		fun printHeap( [], _ ) = ()
		|	printHeap( Node(r,k,d,T)::Ts, tabs ) =
			(print tabs; print (Int.toString k);print " ";print (Int.toString d); print "\n"; 
				printHeap(T, tabs ^ "    ");
			printHeap(Ts,tabs));
		end
end