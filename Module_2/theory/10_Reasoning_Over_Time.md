<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
<script type="text/x-mathjax-config"> MathJax.Hub.Config({ tex2jax: {inlineMath: [['$', '$']]}, messageStyle: "none" });</script>

# Reasoning over Time
`Reasoning about the past predicting the future`.

## 1. Introduction
We live in a temporal dimension, our environment changes as times goes by. Up to now, we deal only within categories, substances and objects, providing us a static perception; we would like to represent a system that **evolves** along the temporal dimension.

We might need to take in account also a **dynamic dimension**, related to the passing time.

There exist two main approaches to describe the dynamical evolution:
- **Event Calculus**.
- **Allen's Temporal Logic**.

## 2. Propositional logic
The main idea is to use **propositional logic** to represent an agent's beliefs about the world. Introducing this kind of logic, it's possible to describe the current state of the world as set of **propositions**. Inside the set should appear only true propositions, thus representing what the agent believes to be true.

Different sets of propositions, put in some order, would represent the world evolution perceived by an agent.

### Example
A child has a bow and an arrow. He can shoot the arrow, throw away the bow, but he can also run, hide and crouch. Our agent observes the child, and it believes that: 
$$\operatorname{KB}^0 = \{\operatorname{hasBow}^0, \operatorname{hasArrow}^0\}$$

The child shoots the arrow, our agent observes the child and it belivies:
$$\operatorname{KB}^1 = \{\operatorname{hasBow}^0, \operatorname{hasArrow}^0, \operatorname{hasBow}^1, \neg \operatorname{hasArrow}^1\}$$

Some observations are already coming out:
- 1<sup>st</sup>. The knowledge base at the time instant $0$ still available for the knowledge base at time instant $1$.
- 2<sup>nd</sup>. If we take, any time, all the knowledge of the previous steps, our agent can easily enter in an infinite search space.
- 3<sup>rd</sup>. We take this last assumption for granted, even though it is not a feasible observation in a real world application (we don't have the capability to store so much informations).

#

What's really interesting is given by the dynamic evolution that bring us from the $\operatorname{KB}^0$ to the $\operatorname{KB}^1$. *What happened in between*? 

There is a general concept about **action**, whose execution changes the world. We would like to describe which are the **effects** of the executed actions.

Additionally, there is a notion of **state** that captures the world at a certain instant. The evolution of the world is given by a sequence of states.

Given the clue about **action**, we need to define a formalism about its effects that could change the world, also called **Effect Axiom**.

### Example

What does it mean *shooting an arrow*? 
$$\operatorname{shoot}^t \rightarrow (\operatorname{hasArrow}^t \Leftrightarrow \neg \operatorname{hasArrow}^{(t+1)})$$

If the action $\operatorname{shoot}$ is executed at time instant $t$, this implies that at time instant $t$ our agent has the arrow and at next time instant $(t+1)$ it will not have anymore the arrow.

$\operatorname{KB}^0 = \{\operatorname{hasArrow}^0\}$ \
$\operatorname{KB}^1 = \{\operatorname{hasArrow}^0, \neg \operatorname{hasArrow}^1\}$

#

...