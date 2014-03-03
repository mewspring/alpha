use "grid.sml";
use "pretty.sml";
use "pathfinder.sml";
use "tilegrid.sml";

(*val tilegrid = TileGrid.merge( TileGrid.make( "demo/floor.txt"),TileGrid.make( "demo/objects.txt") );*)
val tilegrid = TileGrid.make( "demo/myTerrainData.txt" );
val grid = Grid.make ( tilegrid, "demo/walk.txt");

val start = (9,21);
val goal = (58,54);

print("=== [ Example 1 - A* ] ===\n");

val graph = Pathfinder.aStarGraph grid;

val timer = Timer.startRealTimer();
val shortestPath = Pathfinder.aStar(graph, start, goal);
print ("A* took "^(Time.toString(Timer.checkRealTimer(timer)))^" seconds\n");
print ("Path length "^(Int.toString (List.length(shortestPath)))^"\n");

Pretty.printPath(Grid.toArray2 grid, start, goal, shortestPath);

print("=== [ Example 2 - Dijkstra ] ====");

val graph = Pathfinder.dijkstraGraph grid;

val timer = Timer.startRealTimer();
val shortestPath = Pathfinder.dijkstra(graph, start, goal);
print ("Dijkstra took "^(Time.toString(Timer.checkRealTimer(timer)))^" seconds\n");
print ("Path length "^(Int.toString (List.length(shortestPath)))^"\n");

Pretty.printPath(Grid.toArray2 grid, start, goal, shortestPath);
