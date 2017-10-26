#!/usr/bin/python3

def point_sum(seqs,limit):
	count =0
	retval = seqs[0]
	for s in seqs[1:]:
		aux_retval = []
		for z in retval:
			for k in s:
				cand = z+k	
				if cand > limit:
					break
				elif cand == limit:
					count += 1
					break
				else:
					aux_retval.append(z+k)		
		retval = aux_retval
	return retval,count

def cons_seq(denom,amount):
	import math
	retval = []
	for i in range(math.floor(amount/denom)+1):
		retval.append(denom*i)
	return retval

def count_change(amount,denoms):
	seq_list = []
	for d in denoms:
		seq_list.append(cons_seq(d,amount))
	
	k,c = point_sum(seq_list,amount)	
	return c

print(count_change(700,[1,5,10,25,50]))
#print(count_change(700,[50,25,10,5,1]))
