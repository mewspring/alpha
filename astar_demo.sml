use "tilegrid.sml";
use "grid.sml";
use "astar.sml";
use "pretty.sml";

val tilegrid = TileGrid.merge(TileGrid.make("demo/floor.txt"),TileGrid.make("demo/objects.txt"));
val grid = Grid.make(tilegrid, "demo/walk.txt");
val start = (2, 2);
val goal = (2, 12);

(* A* *)

val graph = AStar.makeGraph grid;
val shortestPath = (AStar.find AStar.diagonal)(graph, start, goal);
val processed = !AStar.processedNodes;

print("\n=== [ A* ] ===\n");
Pretty.printPath (Grid.toArray2 grid, start, goal, valOf shortestPath);
print ("A* processed "^(Int.toString(processed))^" nodes\n");
