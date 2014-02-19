alpha
=====

Path finding implemented in a functional programming language

INPUT
-----

* An ascii-file containing the map. It's described by a n*m grid where each cell also has an index to describe the content in that cell.
* An ascii-file listing the index of content a character can travel on.
* The cell index to start travel from
* The cell index which is the goal

Example input:

map.txt
```
7,7,7,7,7
7,5,5,5,7
7,5,2,2,7
7,5,2,5,7
7,5,4,5,7
7,7,7,7,7
```

flyingcreature.txt
```
5 (*floor*)
4 (*chair*)
```

```
Startindex: 6  (*The top left cell of 5 in map.txt*)
Goalindex: 23  (*The bottom right cell of 5 in map.txt*)
```

PROCESS
-----
Convert input into a graph of nodes, where each node is a cell, with bi-directional links between cells that character can traverse.

Find the shortest route in that network between start and goal



OUTPUT
-----
Print to stdout the indicies of the cells to traverse to reach target goal

