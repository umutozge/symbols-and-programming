Question-Answer Sheet for COGS 502
==================================

Last updated: Tue Apr 7 12:17:34 +03 2020


<!-- * 3.7 accumlator version and 3.8 -->

1. In the video `00_conses-and-lists`, at the 5th minute, you say `(cons 3 (cons 2 6))` gives `(3 (2.6))`. Is this a mistake?   
<br/>Yes, definitely; the correct value is `(3 2 . 6)`.<br/><br/>
1. Why isn't there a dot after three in the value of `(cons 3 (cons 2 6))`?  
<br/>Dot is used only to indicate that the final cons is not with `NIL`.<br/><br/>
1. Which expression evaluates to `(3 (2 . 6))`?  
<br/>This is consing 3 to the *list* consisting of the single element `(2 .  6)`; you can construct this element with `(cons 2 6)`, then you can construct the list including only this dotted list as an element by doing `(cons (cons 2 6) nil)`, finally you can cons 3 to this list by `(cons 3 (cons (cons 2 6) nil))`, which is the answer to your question.<br/><br/>
1. What is the cons tree of `(((a (b (x) d))))`? How do the cons tree of `((A B) (C D))` and  `((A B) C D)` differ?  
<br/>Download the program [draw-tree.lisp](../var/draw-tree.lisp) and load it; give your list as an argument to `DRAW-TREE` procedure. Note that a branch to `NIL` is designated by `/`.<br/><br/>
1. Why and when should one prefer a recursive procedure with an accumulator to the one without an accumulator?   
<br/>Please see the video [here](http://lfcs.ii.metu.edu.tr/var/vid/cogs502/42_efficiency-accumulators.m4v)<br/><br/>
1. Can one do object oriented programming in `Common Lisp`?  
<br/>Yes. Have a look at `CLOS` (Common Lisp Object System).<br/><br/>
1. What is symbolic computation?  
<br/>In symbolic computation you manipulate expressions, that consist ultimately of symbols, parantheses, commas, etc. Examples are manipulating logical or arithmetical expressions to simplify them; taking derivatives/integrals by manipulating mathematical expressions, constructing plans by manipulating logical or other formal expressions, and so on. In numerical computation, you compute mathematical functions that involve numbers as input and output.<br/><br/>
1. In `COND` we usually use `T` as the test for the "otherwise" case, can we use other things at that position?  
<br/>In `LISP` every expression can be used as a test: if the expression evaluates to `NIL`, the test fails; in every other case, the test succeeds. Therefore, any expression that is guaranteed NOT to evaluate to `NIL` is as good as `T` as the "otherwise" clause of `COND`. But the custom, which I advise to follow here, is to use `T`.<br/><br/>
1. In defining procedures on lists, we usually give the name `lst` to the parameter that will bind to a list; does the name `lst` have any significance?   
<br/>Absolutely none; any name is as fine as the other -- just don't use symbols that have special meaning to `LISP`.<br/><br/>
