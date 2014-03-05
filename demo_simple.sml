use "grid.sml";
use "pretty.sml";
use "pathfinder.sml";
use "tilegrid.sml";

(* Preprocess of grid and graph. *)

val tilegrid = TileGrid.merge(TileGrid.make("demo/floor.txt"),TileGrid.make("demo/objects.txt"));
val grid = Grid.make(tilegrid, "demo/walk.txt");
val start = (2, 2);
val goal = (2, 12);

(* A* *)

val aStar_graph = Pathfinder.aStarGraph grid;
val aStar_shortestPath = Pathfinder.aStar(aStar_graph, start, goal);

(* Dijkstra *)

val dijkstra_graph = Pathfinder.dijkstraGraph grid;
val dijkstra_shortestPath = Pathfinder.dijkstra(dijkstra_graph, start, goal);

print("\n=== [ A* ] ===\n");
Pretty.printPath (Grid.toArray2 grid, start, goal, valOf aStar_shortestPath);

print("\n=== [ Dijkstra ] ===\n");
Pretty.printPath (Grid.toArray2 grid, start, goal, valOf aStar_shortestPath);
