::: center
:::

::: questions
Remember that `REDUCE` takes a binary procedure and a list. It works
from left to right by first applying the procedure to the first two
elements, then applies the procedure to the value it obtained from its
first application and the third item, then applies the procedure to the
value it obtained in the previous step and the fourth item, and so on.

::: lispcode
(reduce #'+ '(1 2 3)) =\> 6 (reduce #'(lambda (x y) (expt x y)) '(2 2
2)) =\> 16 (reduce #'append '((a b c) (d e f) (g e h))) =\> (A B C D E F
G E H)
:::

Normally `REDUCE` starts computation by applying the given procedure to
the first two items. You may also give a keyword argument
`:INITIAL-VALUE` to make `REDUCE` start the computation by first
applying the procedure to this initial value and the first element in
the list. For instance,

::: lispcode
(reduce #'append '((a b c) (d e f) (g e h)) :initial-value '(x y z)) (X
Y Z A B C D E F G E H)
:::

What should be the procedure in the following expression to get the
given result:

```common-lisp
(reduce \<PROCEDURE\> '(a b c) :initial-value nil) =\> ((A) (B) (C))
```

For a given non-negative integer, the Fibonnacci function is defined as:

$$\begin{aligned}
Fib(n) =  
\begin{cases}
1, & \text{if } n = 0 \text{ or } n=1 \\
Fib(n -1) + Fib(n-2), & \text{otherwise}\\ 
\end{cases}
\end{aligned}$$

It is straightforward to express this definition as a `LISP` procedure:

```common-lisp
(defun fibo (n) (if (\<= n 1) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))
```

But, as we saw in class, this procedure is highly inefficient since it
computes the same value multiple times -- it is impractical to compute
any Fibonacci number greater than 40 in a standard laptop.

An efficient algorithm is the one that is expressed in the following
procedure, which uses `DO`:

::: lispcode
(defun fibodo (n) (do ((i 1 (+ i 1)) (f1 1 (+ f1 f2)) (f2 1 f1)) ((= i
n) f1)))
:::

Define a procedure that implements this algorithm without using `DO`,
`DOLIST`, `DOTIMES`, or a similar iterator. Use recursion with
accumulation.

Give the values of the following expressions. In cases where you think
the expression will give an error, write 'E':

Define a procedure FLATTEN, which takes a possibly nested list and
returns a version where all nesting is eliminated. E.g.,

::: lispcode
(flatten '((1 (2) 3) 4 (((5) 6) 7))) =\> (1 2 3 4 5 6 7)
:::
:::
