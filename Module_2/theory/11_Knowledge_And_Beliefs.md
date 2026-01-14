<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
<script type="text/x-mathjax-config"> MathJax.Hub.Config({ tex2jax: {inlineMath: [['$', '$']]}, messageStyle: "none" });</script>

# Knowledge and Beliefs
`Sharing a common knowledge along the evolution of the time`.

## 1. Introduction
In this chapter we introduce the third mechanism used to reason about the dynamical evolution of a system, and it came out from the **modal logics**. 

The set of modal logics was an attempt for representing agents inside a world, without **complete knowledge**. Each agent might have a different knowledge and, until now, it does not exist any notion of common knowledge. 

However, we would like to describe contact points between agents: they saw same events inside the same environment, leading us to the formalization of shared knowledge.

First Order Logic is a great tool for describing knowledge, even though it misses useful shortcuts for dealing with concepts such as knowledge, beliefs, wishes.

## 2. Knowledge and Beliefs
So far, we discussed about representing knowledge and we adopted this point of view: our agent do some actions based of the knowledge available in the environment.

However, this assumption has some limits: our agent has a knowledge base but it doesn't know it; it does not have any consciussness about its knowledge. It would be great if agents had the capability to retrieve informations from each other.

We do not have the logical stuff to represent consciussness, but we can try using **modal logics**. Usually modal logics are used to define the **propositional attitudes** of an agent, such as: **Believes**, **Knows**, **Wants**, **Informs**. For each of them we have a **Modal Operator**.

The basic idea was to introduce new, special operators, called Modal Operatos, that behaves differently from logical operators seen so far. 

We can summarize modal logics in few steps:
- 1<sup>st</sup> It has the same syntax as FOL.
- 2<sup>nd</sup> It uses modal operators.
- 3<sup>rd</sup> Modal operators take input sentences.
- 4<sup>th</sup> Each modal operator takes inside its scope: the name of an agent and a sentence.
- 5<sup>th</sup> Semantics is extended with the notion of **possible worlds**.
- 6<sup>th</sup> Possible worlds are related through an **accessibility relation**.

### Example

$$\textbf{K}_aP$$

The agent $a$ knows the sentence $P$. $\textbf{K}$ is the propositional attitude **Knows**.

#

## 3. Modal Logics
Modal logics is a family of logics, as description logics, where different types of modal operators has been introduced in order to represent attitudes.

However, we are not really interested about these operators; we introduced them in order to give a clue about accessibility between possible worlds: an agent *knows* *p* if in any accessible world, accessible from the current one, **p is true**.

Given a notion of world, we don't know how it will evolve, but right now we have the tools to access to the informations of the future worlds.

Like any logic, we have to discuss about the axioms of the modal logics. There are different axioms for each modal operator, we focus on the Knows modal operator ones, and, also, it's up to us to add new ones if we think they are strictly necessary.

- 1<sup>st</sup>. Axiom A0. All instances of propositional tautologies are valid.
- 2<sup>nd</sup>. Modus Ponens. If $\phi$ is valid and $\phi \rightarrow \psi$ is valida, then $\psi$ is valid.
- 3<sup>rd</sup>. Distribution Axiom A1. 
  $$K_i\phi \wedge K_i(\phi \rightarrow \psi) \rightarrow K_i\psi$$
  In other words: if the agent $i$ knows $\phi$ and the implication $\phi \rightarrow \psi$ is valid, then the same agent knows also $\psi$. This axiom shows how could be dangerous the modal operator **Knows**: we can deduce anything from the implication of sentences **considered** true.
- 4<sup>th</sup>. Knowledge Generalization Rule. For all the worlds $M$, if $M \models \phi$, then $M \models K_i\phi$. It says: if $\phi$ is true in all the possible worlds, then for sure the agent $i$ **knows** $\phi$.
  
  This axiom is another example about the dangerousness of the **Knows** modal operator. We need a weaker operator in order to avoid logical incompatibilities.
- 5<sup>th</sup>. Knowledge Axiom A2. If an agent knows $\phi$, then $\phi$ is true
  $$K_i \phi \rightarrow \phi$$
  But, *is it always true*? *Shall we say always that $\phi$ is true if the agent $i$ knows $\phi$*? The real answer is: it depends.
- 6<sup>th</sup>. Positive Introspection Axiom A3. 
  $$K_i \phi \rightarrow K_i K_i\phi$$
- 7<sup>th</sup>. Negative Introspection Axiom A4.
  $$\neg K_i \phi \rightarrow K_i \neg K_i\phi$$

The agent might have a partial knowledge of the world, but with these axioms we  get sure that at least it has a perfect knowledge about its own knowledge.

## 4. Linear-time Temporal Logic
Given the amount of the previous modal operators, only few of them are really interesting: they allow us to handle what will be true in the temporal dimension given a dynamic evolution.

**LTL** maps the accessibility relation between worlds into the temporal dimension. Each of them is labelled with an integer, usually a pedix, meaning a time instant.

Any world has two possible ways to evolve:
- **Linear-time**. From each world, there is only one other accessible world.
- **Branching-time**. From each world, many worlds can be accessed.

Temporal logics introduce the following operators:**Allen's Logic**
- $\circ\phi$. Something is true at the next moment in time.
- $\square\phi$. There is a $\phi$ that is always true in the future. Now $\phi$ might be true o false, but at the next moment it will be surely true.
- $\diamond\phi$. There is a $\phi$ that is true sometimes in the future. Sooner or later it will become true.
- $\phi U \psi$. There exists a moment when $\psi$ hold and $\phi$ will hold from now until this moment s (also called **strong until**).
- $\phi W \psi$. $\phi$ will hold from now on unless $\psi$ happens, in which case $\phi$ will cease. Here, we are not sure that $\psi$ will occur in the future (also called **weak until**).

LTL is the third mechanism for reasoning over time; previously we saw **Event Calculus** and **Allen's Logic**. It's quite effective for modelling distributed systems, where different agents can exchange messages and information, and, also, for modelling checking systems (checking if a system, given a certain world, contains or not some stuff).