use "grid.sml";
use "pathfinder.sml";
use "pretty.sml";

val grid = Grid.make ("demo/floor.txt", "demo/objects.txt", "demo/walk.txt");

print("=== [ Example 1 - A* ] ===\n");

val navgrid = Pathfinder.aStarGrid grid;

val start = (2, 3);
val goal = (12, 2);
val shortestPath = Pathfinder.aStar(navgrid, start, goal);

Pretty.printPath(grid, start, goal, shortestPath);

print("=== [ Example 2 - Dijkstra ] ====");

val navgrid = Pathfinder.dijkstraGrid grid;

val start = (2, 3);
val goal = (12, 2);
val shortestPath = Pathfinder.dijkstra(navgrid, start, goal);

Pretty.printPath(grid, start, goal, shortestPath);
