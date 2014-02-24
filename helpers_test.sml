use "helpers.sml";

print("--- [ getIntList ] -------------------------------------------------------------\n");

val test = (1, getIntList "1,2,3" = [1,2,3]);
val test = (2, getIntList "10,foo,20" = [10,20]);
