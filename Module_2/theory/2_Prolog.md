## Prolog 

`A`**`Prolog`**`program is a set of definite clauses.`

## 1. Syntax
But, what is a clause? A clause is set of different logic terms, which typically are:
- __Variables__: strings starting with a __uppercase__ letter.
- __Constants__: numbers or strings starting with a __lowercase__ letter.
- __Atomic formulas__: defined as `p(t1, t2, ..., tn)` where _p_ is a predicate.
- __Compound terms__: known also as _structures_, they are defined similary to a traditional function `f(t1, t2, ..., tn)`, where _f_ is a __function symbol__ and _t1, t2, ..., tn_ are __terms__.

In this way they seem to be so difficult to understand, let's consider a more intuitive approach.
```prolog
X, X1, Goofey, _goofey, _x, _ % variables, the underscore symbol "_" is usually used for variables.

a, goofey, aB, 9, 135, a92 % constants.

p, p(a, f(x)), p(y), q(1) % atomic formulas.

f(a), f(g(1)), f(g(1), b(a), 27) % compound terms.
```

Additionally to the key elements of __Prolog__, we have also different types of clauses:
- __Fact__: ```A.``` represents a statement which is always true.
- __Rule__: ```A :- B1, B2, ..., Bn``` ```A``` is true if and only if ```B1, B2, ..., Bn``` is true.
- __Goal__: ```:- B1, B2, ..., Bn``` it's a question asked to the system.

```prolog
q. % fact

p :- q, r % rule

r(z). % fact

p(x) :- q(X, g(a)) % rule
```

The comma symbol `,` represents the logical __conjuction__ $\land$, instead the neck symbol `:-` defines the implication $\leftarrow$, read from the right to the left.

## 2. Declarative and procedural interpretations
Any Prolog program has two main interpretations, divided into:
- __Declarative interpretation__: the declarative interpretation explains what the program means. Variables within a clause are universally quantified. For each fact:
    ```prolog
    p(t1, t2, ..., tn).
    ```
    If `X1, X2, ..., Xn` are the variables appearing in `t1, t2, ..., tn` the intended meaning is: `∀X1, ∀X2, ..., ∀Xn` the fact `p(t1, t2, ..., tn)` is verified.

    The meaning changes when we talk about __rules__. For each rule: 
    ```prolog
    A :- B1, B2, ..., Bn.
    ``` 
    If `Y1, Y2, ..., Yn` are the variables appearing only in the body of the rule the intended meaning is: `∀X1, ∀X2, ..., ∀Xn ((∃Y1, ∃Y2, ..., ∃Yn (B1, B2, ..., Bn))) → A))`, in other words for each variable `Xi` if exists variable `Yi` the head of the clause `A` is verified. Let's see an example for a better understanding.
    ```prolog
    happyperson(X) :- has(X, Y), car(Y) % for each person X, if exists a car Y (anyone) that X holds, X is a happy person.
    ```
    If `X1, X2, ..., Xn` are the variables appearing in both the body and the head of the rule the intended meaning is: `∀X1, ∀X2, ..., ∀Xn ∀Y1, ∀Y2, ..., ∀Yn ((B1, B2, ..., Bn) → A)`, in other words for each variable `Xi` and variable `Yi`, which made the body verified, the head of clause `A` it's also verified.
    ```prolog
    father(X, Y). % defining the facts of the universe described.
    mother(X, Y).

    grandfather(X, Y) :- father(X, Z), father(Z, Y) % rules heavily dependent on facts.
    grandmother(X, Y) :- mother(X, Z), mother(Z, Y)
    ```
- __Procedural interpretation__: the procedural interpretation of a Prolog program explains how the system executes a goal, in contrast to the declarative interpretation, which only explains what the program means. A procedure is a set of clauses with the same head and the same number of parameters required, also called __arity__. Prolog adopts the __SLD resolution process__ and it has two main charateristics, which are:
    - It selects the __left-most__ literal in any possible query.
        ```prolog
        ? :- G1, G2, ..., Gn. % starting from G1 and then move on.
        ```
    - It performs __Depth-First Search__ strategy. According to the search strategy selected, the order of the clauses in the program may greatly affect the termination, and consequently the completeness. DFS is an __optimal__ search strategy, even though it does not guarantee completeness if the search tree used has inner loopy paths. 
        ```prolog
        p :- q,r.
        p.
        q :- q,t.

        ?- p.
        loopy path
        ```
        In this toy example the fact `p.` comes after the rule `p :- q, r.`. If we ask to the Prolog interpreter the query `?- p.`, the program will enter in a loopy path, failing to solve it. Instead defining the correct order, the query will be immediately solved.
        ```prolog
        p.
        p :- q,r.
        q :- q,t.

        ?- p.
        yes
        ```
        There may exist multiple answer for a query. The way to get them it's really easy: after we get an answer, we could force the interpreter to search the __next solution__. Pratically, it means to ask to the procedure (a set of clauses with the same head and arity) to explore the remaing part of the __search tree__. In Prolog the standard way consists on using the operator `;`.
        ```prolog
        :- sister(maria, W).
        yes W = giovanni;
        yes W = annalisa;
        no
        ```
        The knowledge `giovanni, annalisa` comes from the facts previously defined.

## 3. Royal family exercise

## 4. Arithmetic and math in Prolog
Arithmetic in Prolog is not a standard logical feature; it relies on special built-in predicates to force them evaluation. In Prolog an expression like `2 + 3` is just a __term__, not the numeric value `5`. For instance, the interpreter will associate to the fact `p(2 + 3).` the structure `+ (2, 3)`. 

However, the special and pre-defined predicate `is` forces the evaluation of any mathemical expression. Its syntax is really easy, we have to define the variable, then the predicate `is` and finally the expression to evaluate.
```prolog
T is Expr
```
Previously, we said that Prolog is based on the SLD resolution process, it always evaluates the left-most literal. But this is not the case. The predicate `is` forces the interpreter to evaluate the right-most literal (it matches the mathematical expression) and the final result will be associated to the variable in the next step.
```prolog
?- X is 2 + 3.
yes X = 5

?- X1 is 2+3, X2 is exp(X1), X is X1 * X2.
yes X1 = 5, X2 = 148.143, X = 742.065

?- X is Y - 1.
No
(or Instantion Fault, depending on the prolog system)
```
```prolog
?- X is 2 + 5, X is 4 + 1.
yes X = 5
```
In this example, the second goal becomes:
```prolog 
:- 5 is 4 + 1.
```
X has been instantiated by the evaluation of the first goal. As before, the order of the goal is very important:
```prolog
(a) :- X is 2 + 3, Y is X + 1.
(b) :- Y is X + 1, X is 2 + 3.
```
Goal `(a)` succeeds and returns `yes X = 5, Y = 6`; instead the goal `(b)` fails, according to the incorrect order defined (Y comes before the evaluation of the mathematical expression of X).

A term representing an expression is evaluated only if it is the argument of the predicate `is`. For instance:
```prolog
p(a, 2 + 3 * 5).
p(b, 2 + 3 + 5).
q(X, Y) :- p(a, Y), X is Y.

(q(X, Y) :- p(_, Y), X is Y.) % this clause will use both procedures, achieved by the anonymus symbol.

?- q(X, Y)
yes X = 17 Y = 2 + 3 * 5 (Y=+(2, *(3, 5)))
```
Initially, the predicate `p(a, Y)` is unified with the fact `p(a, 2 + 3 * 5).`. The association defines the atomic structure `+(2 *(3, 5))`. In the second step occurs the evaluation of the mathematical expression `X is 2 + 3 * 5`. (Why do we define the constant `a` inside the fact __p__? As we already know, a procedure is a set of clauses with same head and arity, the constant `a` allow us to distinguish which predicate we have to obtain!)

Additionally, it's also possible to compare expressions results by the standard __relational operators__, which are: `>, <, >=, =<, ==, =/=`. The last two operators are named respectvely __arithmetically equal to__ (`==`) and __arithmetically not equal to__ (`=/=`). The syntax is pretty similar to the predicate `is`.
```prolog
Expr1 REL Expr2
```
`REL` is the relational operator, `Expr1` and `Expr2` are the evaluated expressions. It's crucial that both the expressions are completely instantiated before the comparison, otherwise the Prolog program will fail.
```prolog
p(a, 2 + 3 * 5).
p(b, 2 + 3 + 5).

comparison_values(V1, V2, equal) :- V1 == V2.
comparison_values(V1, V2, first_value_greater) :- V1 > V2.
comparison_values(V1, V2, second_value_greater) :- V1 < V2.

comparison_expressions(Type1, Type2, Result):- 
    p(Type1, Expr1),
    p(Type2, Expr2),
    Value1 is Expr1,
    Value2 is Expr2,
    comparison_values(Value1, Value2, Result).

?- comparison_expressions(a, b, R).
```
Right now we have all the necessary ingredients to build __math functions__ in Prolog. Given any function `f` with a certain arity `n`, we can implement it through a `(n + 1)` predicate. Given the function $f:x_1, x_2, ..., x_n \rightarrow y$ it's represented by a predicate as follows: `f(X1, X2, ..., Xn, Y)`; we must always indicate the result variable `Y` within the predicate scope, in this way the interpreter knows exactly what the output will be.
```prolog
fatt(0, 1).
fatt(N, Y) :- 
    N > 0,
    N1 is N - 1,
    fatt(N1, Y1),
    Y is N * Y1.
```
Before moving on, it's crucial to understand its behavior and how it does really work. The example above uses the __recursion__  to solve the `factorial problem`: it begins constructing the search tree until it reaches the leaf nodes (`fact(0, 1)`) and it goes forward to the root node computing the mathematical expression at each step.

## 5. Iteration and recursion
In Prolog __does not exist__ the iteration, like `while, foreach or repeat`. But, we can simulate the iterative behavior through __recursion__, as already done in the `factorial example`. Prolog models iteration by defining a predicate (Remember: a predicate is a set of clauses, not just one!) with two essential parts:
- __Base case__: A non-recursive clause, which is generally a __fact__, that defines the __termination condition__ of the process.
- __Recursive case__: A rule that performs a single step of the operation and then calls itself with modified arguments, moving the process closer to the base case.
```prolog
print(1) :- write(1), nl.
print(N) :- 
    N > 1,
    write(N - 1), nl,
    N1 is N - 1,
    print(N1).
```
The `print(N)` predicate is different from the `factorial(N, Y)` predicate; if previously the interpreter completed the search tree before computing the expressions, right now it immediately shows the results through the `write` predicate. 

Even tough any well-structured recusion works fine, a specific type of it is more desirable for effiency: __tail recursion__. A function is tail-recursive if the recursive call is the __most external call__ in its definition. There are so many cases where a non-tail recursion can be re-written as a tail recursion.
```prolog
fatt1(N, Y):- fatt1(N, 1, 1, Y).
fatt1(N, M, ACC, ACC) :- M > N.
fatt1(N, M, ACCin, ACCout) :- 
    ACCtemp is ACCin * M,
    M1 is M + 1,
    fatt1(N, M1, ACCtemp, Accout).
```
The factorial is computed using an __accumulator__. An accumulator is an extra argument passed to the predicate, which holds the running or partial result of the computation at each step. The main advantage is that the evaluation of the mathematical expression is done __before__ the recursive call, avoiding __backtracking__ and mantaining a constant __space complexity__.

## 6. Iteration exercises

## 7. Lists
Lists are one of the most fundamental and widely used data structures in any possible programming languages. In Prolog, lists are terms build upon the special atom `[ ]`, called __empty list__, and the __constructor__ operator `.`. A list is recursively defined as:
- The __empty list__, `[ ]`.
- A non-empty list consisting of a __head__ and a __tail__, where the tail is itself a list, `.(T, List)`.
<div align="center">
    <table>
        <head>
            <th>Standard notation</th>
            <th>Head-Tail notation</th>
        </head>
        <body>
            <tr align="center">
                <td>[a]</td>
                <td>.(a, [])</td>
            </tr>
            <tr align="center">
                <td>[a, b]</td>
                <td>.(a, .(b, []))</td>
            </tr>
            <tr align="center">
                <td>[a, b, c]</td>
                <td>.(a, .(b, .(c, [])))</td>
            </tr>
        </body>
    </table>
</div>

Since the __head-tail notation__ might be quite difficult to use, the term `.(T, List)` can be also represented as `[T | List]`. Once again, the __head__ is `T` and the __tail__ is `List`.
<div align="center">
    <table>
        <head>
            <th>Standard notation</th>
            <th>Head-Tail notation</th>
        </head>
        <body>
            <tr align="center">
                <td>[a]</td>
                <td>[a | [ ]]</td>
            </tr>
            <tr align="center">
                <td>[a, b]</td>
                <td>[a | [b | [ ]]]</td>
            </tr>
            <tr align="center">
                <td>[a, b, c]</td>
                <td>[a | [b | [c | [ ]]]]</td>
            </tr>
        </body>
    </table>
</div>

Even in this case, the recursive notation `[T | List]` is rather __verbose__. Therefore, we can use a more simplified syntax version, such as `[a, b, c]` for the term `[a | [b | [c | [ ]]]]`. 

The greatest power about lists in Prolog comes from the easy way to manipulate them using an __unification algorithm__. This provides a complete method for accessing and deconstructing list content.
```prolog
p([1, 2, 3, 4, 5, 6, 7, 8, 9]).

:- p(X).
yes X = [1, 2, 3, 4, 5, 6, 7, 8, 9]

:- p([X|Y]).
yes X = 1 Y = [2, 3, 4, 5, 6, 7, 8, 9]

:- p([X,Y|Z]).
yes X = 1 Y = 2 Z = [3, 4, 5, 6, 7, 8, 9]

:- p([_|X]).
yes X = [2, 3, 4, 5, 6, 7, 8, 9]
```
This snippet of code represents some examples about list unification processes. In particular, it's important to focus on the last predicate figured: we used the anonymus symbol to create a new list containing all the previous values except the first one. The __anonymus symbol__ allow us to "delete" somehow the first value of the current data structure.

List operations are inherently recursive, using the `[Head|Tail]` data structure to process one element at a time, until the base case `[]` (empty list) is reached. Many predicates about `list` can be written using this rule as main knowledge.

1. __isList__

    `isList` checks if an argument is a list, the base case is the empty list `[]`, and the recursive part checks if the tail is also a list. 
    ``` prolog
    isList([]).
    isList([X|Tail]) :- isList(Tail).

    :- isList([1, 2, 3]).
    yes 

    :- isList([a|b]).
    no
    ```

2. __member__

    `member` checks if an element is in a given list, the base case is when the element coincide with the head of the list, and the recursive case checks if the element is in the tail of the list.
    ```prolog
    member(X, [X| ]).
    member(X, [_|Tail]) :- member(X, Tail).

    :- member(1, [1, 2, 3]).
    yes 

    :- member(4, [1, 2, 3]).
    no

    :- member(X, [1, 2, 3]).
    yes X = 1;
        X = 2:
        X = 3;
    no
    ```

3. __length__

    `length` defines the size of the list, it takes as first argument a list and the second argument is the number of elements contained in the list.
    ```prolog
    length([], 0).
    length([_|Tail], N) :- 
        length(Tail, NT),
        N is NT + 1.

    :- length([1, 2, 3], 3).
    yes

    :- length([1, 2, 3], Result).
    yes Result = 3
    ```

4. __append__

    `append` predicate takes three lists as arguments, which the first two are real lists, already instanciated, instead the last argument is the list obtained by concatenating  the two lists. This is a highly reversible and powerful tool. 
    
    1. If the first list is empty, the result is the second list.
    2. If the first list has a `head`, keep it, and recursively, append the rest to the second list.

    ```prolog 
    append([], L1, L1).
    append([H | Rest1], L2, [H | NewTail]) :-
        append(Rest1, L2, NewTail).

    :- append([1, 2], [3, 4, 5], L).
    yes

    L = [1, 2, 3, 4, 5]
    :- append([1, 2], L2, [1, 2, 4, 5]).
    yes

    L2 = [4, 5]
    :- append([1, 3], [2, 4], [1, 2, 3, 4]).
    no
    ```

5. __deleteFirstOccurrence__

    `deleteFirstOccurrence` takes as first and second arguments an element and a list respectively, and the third argument is the list without the first occurrence of the element.

    ```prolog
    deleteFirstOccurence(El, [], []).
    deleteFirstOccurence(El, [El|T], T).
    deleteFirstOccurence(El, [H|T], [H|T1]) :-
        deleteFirstOccurence(El, T, T1).
    ```

6. __deleteAllOccurrences__

    `deleteAllOccurrences` is a predicate that takes as first and second arguments an element and a list, the third parameter is the list without all the terms that unify with the element.

    ```prolog
    deleteAllOccurences(El, [], []).
    deleteAllOccurences(El, [El|T], Result):- deleteAllOccurrences(El, T, Result).
    deleteAllOccurences(El, [H|T], [H|T1]) :- deleteAllOccurences(El, T, T1).
    ```

7. __reverse__ 

    `reverse` taking two lists as arguments, it returns a list that is the reversed of the second given.

    ```prolog
    reverse([],[]).
    reverse([H|T], Result) :-
        reverse(T, Partial),
        append(Partial, [H], Result).

    :- reverse([], []).
        yes

    :- reverse([1, 2], Lr).
        yes Lr = [2, 1]

    :- reverse(L, [2, 1]).
        yes L = [1, 2]
    ```

## 8. List exercises

## 9. The CUT
The __cut operator__ `!` is a pre-defined predicate that allows to interfere and control the execution process of a Prolog program. It has no logic meaning or declarative semantic, but it heavily affects the execution process.

Any execution process is build upon two stacks:

- __Backtracking stack__. It contains the set of open choice points.
- __Execution stack__. It contains the activation record of the predicates.