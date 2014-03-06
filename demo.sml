use "tilegrid.sml";
use "grid.sml";
use "pathfinder.sml";
use "pretty.sml";

(* Preprocess of grid and graph. *)

val tilegrid = TileGrid.merge(TileGrid.make("demo/floor.txt"),TileGrid.make("demo/objects.txt"));
val grid = Grid.make(tilegrid, "demo/walk.txt");
val start = (2, 2);
val goal = (2, 12);

(* A* *)

val aStar_graph = Pathfinder.aStarGraph grid;
val aStar_shortestPath = Pathfinder.aStar(aStar_graph, start, goal);
val aStar_shortestPath = valOf aStar_shortestPath
val astar_processed = !Pathfinder.processedNodes;


(* Dijkstra *)

val dijkstra_graph = Pathfinder.dijkstraGraph grid;
val dijkstra_shortestPath = Pathfinder.dijkstra(dijkstra_graph, start, goal);
val dijkstra_shortestPath = valOf dijkstra_shortestPath
val dijkstra_processed = !Pathfinder.processedNodes;

print("\n=== [ A* ] ===\n");
Pretty.printPath (Grid.toArray2 grid, start, goal, aStar_shortestPath);
print ("Path length "^(Int.toString (List.length(aStar_shortestPath)))^"\n");
print ("A* processed "^(Int.toString(astar_processed))^" nodes\n");

print("\n=== [ Dijkstra ] ===\n");
Pretty.printPath (Grid.toArray2 grid, start, goal, dijkstra_shortestPath);
print ("Path length "^(Int.toString (List.length(dijkstra_shortestPath)))^"\n");
print ("Dijkstra processed "^(Int.toString(dijkstra_processed))^" nodes\n");
