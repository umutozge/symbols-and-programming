#!/usr/bin/env python3

class FiniteAutomaton():
	def __init__(self,\
						alphabet=[1,0],\
						states=set(),\
						transitions={},\
						startState=0,\
						finalStates=set(),\
						tape=None):
		self.alphabet = alphabet	
		self.states = set(states)

		if transitions:
			self.transitions = transitions
		else:
			self.transitions = {}
			for s in states:
				self.transitions[s] = {}	

		self.startState = startState
		self.tape = tape
		self.currentState = self.startState
		self.finalStates = finalState


	def addTransition(self, instate, symbol, outstate):
		val = self.transitions.get(instate)	
		if val:
			val[symbol] = outstate 
		else:
			self.transitions[instate]={symbol:outstate}
	
	def getConfig(self):
		return (self.currentState,self.tape)

	def step(self):
		if tape:
			self.currentState = self.transitions[self.currentState][tape.pop(0)]
			return True
		else:
			return False
	


	def printTable(self):
		trans = self.transitions
		print("State","\tSymbol","State")
		for k in trans.keys():
			for s in trans[k].keys():
				print(str(k),"\t"+str(s),"\t"+str(trans[k][s]))
			

if __name__=="__main__":
	fa = FiniteAutomaton(states=[0,1,2,3],tape=[1,0,0,1])
	fa.addTransition(0,1,1)
	fa.addTransition(0,0,0)
	fa.addTransition(1,1,0)
	fa.addTransition(1,0,1)
	fa.printTable()
