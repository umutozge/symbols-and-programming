#!/usr/bin/python3
def count_change(amount,denoms):
	if amount == 0:
		return 1
	elif amount < 0 or denoms == []:
		return 0
	else:
		return count_change(amount,denoms[1:]) + count_change(amount-denoms[0],denoms)
	
print(count_change(700,[1,5,10,25,50]))
#print(count_change(700,[50,25,10,5,1]))
