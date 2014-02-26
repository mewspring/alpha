structure Pqueue = 
struct
abstype 'a binoTree = Node of int * int * 'a * 'a binoTree list
with
(* Some selectors *)
fun rank (Node(r,k,d,Ts)) = r
fun root (Node(r,k,d,Ts)) = k

val empty = []
fun isEmpty [] = true
  | isEmpty _  = false 

(* From the slides *)
fun link (T1 as Node(r1,k1,d1,Ts1), T2 as Node(r2,k2,d2,Ts2)) =
    if k1 < k2 then
        Node(r1+1,k1,d1,T2::Ts1)
    else
        Node(r1+1,k2,d2,T1::Ts2)

fun insTree ([], T) = [T]
   | insTree (H as T'::H', T) =
     if rank T < rank T' then T::H
     else if rank T' < rank T then T' :: insTree (H', T) 
     else insTree (H', link (T, T'))

fun insert (H, k, d) = insTree (H, Node(0,k,d,[]))

fun merge (H1, []) = H1 
  | merge ([], H2) = H2
  | merge (H1 as T1::H1', H2 as T2::H2') =
    if rank T1 < rank T2 then T1 :: merge (H1', H2)
    else if rank T2 < rank T1 then T2 :: merge (H1, H2')
         else insTree (merge (H1', H2'), link (T1, T2))


fun extractMinTree [T] = (T, [])
  | extractMinTree (T::H) =
    let val (T', H') = extractMinTree H
    in  if root T < root T' then (T, H) else (T', T::H') end

fun minimum H =
    let
        val (T, _) = extractMinTree H
    in
      root T
    end

fun extractMin H =
    let
        val (Node(_,k,d,H1), H2) = extractMinTree H
    in
        ((k,d), merge(rev H1, H2))
    end

fun deleteMin H = 
    let val (_, H') = extractMin H 
    in H'
    end

fun update( [], _, _ ) = []
|	update( Node( r, k1, d1 , T )::Ts, k, d ) =
	let
		fun swap( k1, d1, T, k, d ) =
			let
				val (Node( r2, k2, d2, Ts2)) = T
			in
				if d2 = d andalso k < k1 then (* move node up / swap *)
						( k, d, Node( r2, k1, d1, Ts2))
				else
					( k1, d1, T )
			end
		fun swap'( k1, d1, [], res, k, d ) = ( k1, d1, rev res )
		|	swap'( k1, d1, T::Ts, res, k, d) = 
			let
				val (k2,d2,T1) = swap( k1, d1, T, k, d) 
			in
				if d1 = d then
					if k <= k1 then
						( k, d1, (rev res)@(T::Ts))
					else
						raise Fail "update-> Updating the priority with a higher value is not supported"
				else
					swap'( k2, d2, Ts, T1::res, k, d)
			end
		val (k2,d2,T2) = swap'( k1, d1, update(T,k,d), [], k, d);
	in
		Node(r, k2, d2, T2)::update( Ts, k, d )
	end

fun printQueue( [], _ ) = ()
|	printQueue( Node(r,k,d,T)::Ts, tabs ) =
	(print tabs; print (Int.toString k);print " ";print (Int.toString d); print "\n"; 
		printQueue(T, tabs ^ "    ");
	printQueue(Ts,tabs));
end
end


val testCase = [ (5,3),(12,4),(4,2),(2,1),(13,5),(1,0),(20,6) ];
fun createHeap( [], h ) = h
|	createHeap( (k,d)::xs, h ) = createHeap( xs, Pqueue.insert(h, k, d ))
val q = createHeap( testCase, Pqueue.empty );
Pqueue.printQueue( q, "   ");
