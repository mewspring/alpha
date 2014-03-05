use "helpers.sml";

print("--- [ getIntList ] -------------------------------------------------------------\n");

val test = (1, Helpers.getIntList "1,2,3" = [1,2,3]);
val test = (2, Helpers.getIntList "10,foo,20" = [10,20]);

print("--- [ contains ] ---------------------------------------------------------------\n");

val test = (1, Helpers.contains ([1,2,3], 3) = true);
val test = (1, Helpers.contains ([1,2,3], 4) = false);

use "grid.sml";
use "pretty.sml";
use "pathfinder.sml";
use "tilegrid.sml";

val tilegrid = TileGrid.merge(TileGrid.make("demo/floor.txt"),TileGrid.make("demo/objects.txt"));
val grid = Grid.make(tilegrid, "demo/walk.txt");
val start = (2, 2);
val goal = (2, 12);
val aStar_graph = Pathfinder.aStarGraph grid;
val aStar_shortestPath = Pathfinder.aStar(aStar_graph, start, goal);

print("--- [ Pathfinder.aStar ] -------------------------------------------------------\n");

val test = (1, aStar_shortestPath = SOME [(3, 3), (4, 3), (5, 3), (6, 3), (7, 3), (8, 3), (9, 3), (10, 4), (10, 5), (10, 6), (10, 7), (10, 8), (10, 9), (10, 10), (10, 11), (9, 12), (8, 11), (7, 10), (6, 9), (5, 8), (5, 7), (4, 6), (3, 7), (2, 8), (2, 9), (2, 10), (2, 11), (2, 12)]);
