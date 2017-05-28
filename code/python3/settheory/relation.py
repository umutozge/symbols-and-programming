#
# Diagnostic functions on relations
#

#generate
def generate(a_set):
	"""Takes a set object and returns a random binary relation over this set"""

	# We will need ramdom integers. First let us import the Python
	# module that provides such tools.
	import random
	
	# Let us compute the maximum possible size of a binary relation
	# that can.  be defined over the input set.  The size cannot be
	# larger than the size of the Cartesian product of the set with
	# itself.
	max_size = len(a_set)*len(a_set)

	# Randomly select a size for the relation to be generated
	size = random.randint(0,max_size)

	# Now we will randomly generate the tuples (ordered paris) of our
	# relation To collect the generated tuples, we form an accumulator
	# A set object would be appropriate for the task.  This will be
	# our return value as well

	retval = set()

	# Within a while loop we will generate random tuples and add them
	# to the accumulator. In every iteration we will check whether we
	# generated enough tuples or not. As we have a set accumulator, we
	# don't need to worry about adding more than one instance of the
	# same tuple.
	#

	while len(retval) < size:
		
		# Pick two random elements from the set and bring them
		# together in a tuple.  You do not need to worry about how
		# random elements are picked from a set.
		random_tuple = (random.sample(a_set,1)[0],random.sample(a_set,1)[0]) 	

		# Add the random tuple to the accumulator. As we have a set as
		# accumulator, nothing happens when one attempts to add a
		# tuple already in the accumulator. If the tuple is not alrady
		# there, it is added; and we are one step closer to finish the
		# job. The loop continues until we added enough tuples.
		retval.add(random_tuple)		
	
	# return the accumulator
	return retval

#functionhood

def is_function(a_relation,a_set):

	# first we collect the first componenets of all the pairs in
	# a_relation in a set; while doing it, we also check whether there
	# are any pairs with the same first component. If we observe such
	# a case, then we stop execution by returning False, since we can
	# decide that we don't have a function.
	
	set_of_firsts = []
	for pair in a_relation:
		if pair[0] in set_of_firsts:
			return False
		set_of_firsts.append(pair[0])
	
	# check whether a_relation maps every element of a_set to a value
	# for this first check whether set_of_firsts has a length equal to a_list

	if len(set_of_firsts) != len(a_set):
		return False

	# then check whether everything in a_set is also in set_of_firsts
	# for relation diagnostics we assumed everything in the relation
	# is coming from the set we define the relation on. Here we
	# explicitely check it. 

	for x in a_set:
		if not x in set_of_firsts:
			return False

	# if everything goes well, then decide that the given relation is
	# a function

	return True

# reflexivity

def is_reflexive(a_relation,a_set):
	"""Takes a relation defined over a set"""

	for x in a_set:
		if not [x,x] in a_relation:
			return False
	
	# if the for loop reaches its end, this means the if clause never
	# got True; because otherwise the return statemtn would have been
	# executed and the function would have exited before it reached
	# the end of the for loop. Therefore we can conclude that the
	# relation was reflexive (all pairs [x,x] were in the realtion),
	# and we can return True as the result of the function.

	return True

def is_nonreflexive(a_relation,a_set):

	return not is_reflexive(a_relation,a_set)

def is_irriflexive(a_relation,a_set):

	for x in a_set:
		if [x,x] in a_relation:
			return False

	# if the for loop reaches its end, this means the if clause never
	# got True; because otherwise the return statemtn would have been
	# executed and the function would have exited before it reached
	# the end of the for loop. Therefore we can conclude that the
	# relation was irreflexive (no pairs [x,x]), and we can return
	# True as the result of the function.

	return True

# symmetry

def is_symmetric(a_relation):

	for pair in a_relation:
		if not [pair[1],pair[0]] in a_relation:
			return False
	
	return True

def is_nonsymmetric(a_relation):

	return not is_symmetric(a_relation)

def is_asymmetric(a_relation):

	for pair in a_relation:
		if [pair[1],pair[0]] in a_relation:
			return False
	
	return True

def is_antisymmetric(a_relation):

	for pair in a_relation:
		if [pair[1],pair[0]] in a_relation and pair[1] != pair[0]: 
			return False

	return True


#transitivity

def is_transitive(a_relation):
	
	for x in a_relation:
		for y in a_relation:
			if x[1] == y[0] and not [x[0],y[1]] in a_relation:
				return False
	
	return True

def is_nontransitive(a_relation):

	return not is_transitive(a_relation)

def is_intransitive(a_relation):

	for x in a_relation:
		for y in a_relation:
			if x[1] == y[0] and [x[0],y[1]] in a_relation:
				return False
	
	return True



if __name__ == '__main__':
	base = [1,2,3,4]
	test_relations =[[[1,2],[1,1],[3,4],[3,3],[4,4],[2,2]],\
					[[1,2],[2,1],[3,4],[3,3],[4,3],[2,2]],\
					[[1,2],[1,4],[3,4],[2,3],[4,1],[3,2]],\
					[[1,2],[1,4],[3,4],[2,3],[4,1],[3,1]],\
					[[1,2],[1,4],[3,4],[3,2]],\
					[[1,2],[1,4],[3,4],[4,2]],\
					[[1,4],[3,4],[4,2]],\
					[[1,4],[2,4],[3,2],[4,2]],\
					[[1,4],[2,4],[3,2]],\
					[[1,4],[2,4],[3,2],[4,2],[3,1]],\
					[]]

	for rel in test_relations:
		print rel
# 		print 'Reflexive: ' + str(is_reflexive(rel,base))
# 		print 'Nonreflexive: ' + str(is_nonreflexive(rel,base))
# 		print 'Irreflexive: ' + str(is_irriflexive(rel,base))
# 		print 'Symmetric: ' + str(is_symmetric(rel))
# 		print 'Nonsymmetric: ' + str(is_nonsymmetric(rel))
# 		print 'Asymmetric: ' + str(is_asymmetric(rel))
# 		print 'Anti-symmetric: ' + str(is_antisymmetric(rel))
# 		print 'Transitive: ' + str(is_transitive(rel))
# 		print 'Nontransitive: ' + str(is_nontransitive(rel))
# 		print 'Intransitive: ' + str(is_intransitive(rel))
		print 'Function: ' + str(is_function(rel,base))

		print

