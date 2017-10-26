#!/usr/bin/python3

def count_change(amount,denoms):
	if amount == 0:
		return 1
	elif amount < 0 or denoms == []:
		return 0

	import math
	pivot = denoms[0]
	ceiling = math.floor(amount/pivot)+ 1	
	if ceiling == 1:
		return 0

	change = 0

	for i in range(ceiling):
		change = change + count_change(amount - i*pivot, denoms[1:]) 
	
	return change

print(count_change(700,[1,5,10,25,50]))
#print(count_change(700,[50,25,10,5,1]))
