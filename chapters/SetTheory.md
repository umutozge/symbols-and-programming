# Set Theory

## Sets

### Axiom of Specification and Ruseell's Paradox

The Axiom of Specification states that given any set $A$ and any property
$P(x)$, there is a set $B = \{x \in A\, |\, P(x) \}$.   

Notably, the axiom does not impose any restriction on the property $P(x)$. It was Bertrand Russell, who discovered an important corollary of this lack of
restriction.  Take $P(x)$ as $x\not\in x$. Given an arbitrary set $A$,
the Axiom of Specification guarantees that there is a set $B = \{x\in A\, |\,
x\not\in x\}$. Given that $B$ is a well-defined set, given any object $y$,
it should be possible to answer whether  $y\in B$  or not: for all $y$,
$y\in B$ iff $y\in A$ and $y\not\in y$. Now we can ask whether $B\in B$
or not. If $B\in B$, then $B\not\in B$, therefore we arrive at a
contradiction. We must conclude that $B\not\in B$. Knowing that $B\not\in
B$, then we must conclude $B\not\in A$, since if it were, then $B$ would be
in $B$, leading again to a contradiction.  Therefore, given an arbitrary set
$A$, there is always at least one mathematical object, the set $B$
constructed as above, that is not included in $A$. This shows that there can
be no set that includes everything, there will always be a well-defined object
left out. (Halmos Ch. 2)

## Relations

