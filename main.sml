use "helpers.sml";
use "pathfinder.sml";
use "pretty.sml";

val grid' = Helpers.readGrid("floor.txt", "objects.txt");
val walkable = Helpers.readWalkable "walk.txt";
val grid = Helpers.preprocess(grid', walkable);

val navgrid = Pathfinder.aStarGrid grid;

(*Example *)
val start = (2,3);
val goal = (7,9);
val shortestPath = Pathfinder.aStar( navgrid, start, goal);

Pretty.printPath(grid, start, goal, shortestPath);
