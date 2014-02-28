use "grid.sml";
use "pathfinder.sml";
use "pretty.sml";

val grid = Grid.make ("demo/floor.txt", "demo/objects.txt", "demo/walk.txt");
val navgrid = Pathfinder.aStarGrid grid;

print("=== [ Example 1 ] ===\n");

val start = (2, 3);
val goal = (12, 2);
val shortestPath = Pathfinder.aStar(navgrid, start, goal);

Pretty.printPath(grid, start, goal, shortestPath);
