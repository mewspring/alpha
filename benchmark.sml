use "tilegrid.sml";
use "grid.sml";
use "pathfinder.sml";
use "pretty.sml";

fun repeat( f, arg, 1 ) = f(arg)
|	repeat( f, arg, n ) =
		( f(arg) ; repeat(f,arg,n-1) )

(* Simple *
val tilegrid = TileGrid.merge( TileGrid.make( "demo/floor.txt"),TileGrid.make( "demo/objects.txt") );
val grid = Grid.make ( tilegrid, "demo/walk.txt");
* Simple *)

(* Benchmark *)
val scale = 1; (* Number of times to scale the map *)
val runPathFind = 1; (* Number of times to run the pathfinder *)
val tilegrid = TileGrid.make( "demo/myTerrainData.txt" );
val width = 80;
val height = 60;
val tilegrid = TileGrid.loopExtend( tilegrid, width*(scale-1), height*(scale-1) )
val grid = Grid.make ( tilegrid, "demo/walk.txt");
val start = (9,21);
val goal = ( (width*scale)-22, (height*scale)-6 );
(* Benchmark *)

print("=== [ Example 1 - A* ] ===\n");

val graph = Pathfinder.aStarGraph grid;

val timer = Timer.startRealTimer();
val astar_shortestPath = repeat( Pathfinder.aStar, (graph, start, goal), runPathFind );
val astar_shortestPath = valOf astar_shortestPath
val astar_processed = !Pathfinder.processedNodes;
val astar_time = Timer.checkRealTimer(timer);

(*Pretty.printPath(Grid.toArray2 grid, start, goal, astar_shortestPath); *)

print("=== [ Example 2 - Dijkstra ] ====");

val graph = Pathfinder.dijkstraGraph grid;

val timer = Timer.startRealTimer();
val dijkstra_shortestPath = repeat( Pathfinder.dijkstra, (graph, start, goal), runPathFind );
val dijkstra_shortestPath = valOf dijkstra_shortestPath
val dijkstra_processed = !Pathfinder.processedNodes;
val dijkstra_time = Timer.checkRealTimer(timer);

(*Pretty.printPath(Grid.toArray2 grid, start, goal, dijkstra_shortestPath);*)

print("=== Results ] ====");

print ("A* took "^(Time.toString(astar_time))^" seconds\n");
print ("A* processed "^(Int.toString(astar_processed))^" nodes\n");
print ("Path length "^(Int.toString (List.length(astar_shortestPath)))^"\n");
print ("Dijkstra took "^(Time.toString(dijkstra_time))^" seconds\n");
print ("Dijkstra processed "^(Int.toString(dijkstra_processed))^" nodes\n");
print ("Path length "^(Int.toString (List.length(dijkstra_shortestPath)))^"\n");
