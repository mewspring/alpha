use "grid.sml";
use "pathfinder.sml";
use "pretty.sml";

val grid = Grid.make ("demo/floor.txt", "demo/objects.txt", "demo/walk.txt");

print("=== [ Example 1 - A* ] ===\n");

val graph = Pathfinder.aStarGraph grid;

val start = (2, 3);
val goal = (12, 2);
val shortestPath = Pathfinder.aStar(graph, start, goal);

Pretty.printPath(grid, start, goal, shortestPath);

print("=== [ Example 2 - Dijkstra ] ====");

val graph = Pathfinder.dijkstraGraph grid;

val start = (2, 3);
val goal = (12, 2);
val shortestPath = Pathfinder.dijkstra(graph, start, goal);

Pretty.printPath(grid, start, goal, shortestPath);
