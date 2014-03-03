import array

fin = open(r'E:\ExtDev\alpha\demo\myTerrainData.dat', "rb")

import array
values = array.array('b') # array of long integers
values.read(fin, 4800) # read 1 integer

out = open(r'E:\ExtDev\alpha\demo\myTerrainData.txt', "w")
with out:
	for c in range(80):
		for r in range(60):
			out.write( str(int(not bool(values[(r*60)+c])))+"," )
		out.write("\n")
