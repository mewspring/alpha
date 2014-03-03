use "grid.sml";
use "pretty.sml";
use "pathfinder.sml";

val grid = Grid.make ("demo/myTerrainData.txt", "demo/myTerrainData.txt", "demo/walk.txt");

print("=== [ Example 1 - A* ] ===\n");

val graph = Pathfinder.aStarGraph grid;

val start = (9,21);
val goal = (58,75);

val timer = Timer.startRealTimer();
val shortestPath = Pathfinder.aStar(graph, start, goal);
print ("Pathfinding took "^(Time.toString(Timer.checkRealTimer(timer)))^" seconds\n");

Pretty.printPath(Grid.toArray2 grid, start, goal, shortestPath);

print("=== [ Example 2 - Dijkstra ] ====");

val graph = Pathfinder.dijkstraGraph grid;

val timer = Timer.startRealTimer();
val shortestPath = Pathfinder.dijkstra(graph, start, goal);
print ("Pathfinding took "^(Time.toString(Timer.checkRealTimer(timer)))^" seconds\n");

Pretty.printPath(Grid.toArray2 grid, start, goal, shortestPath);
