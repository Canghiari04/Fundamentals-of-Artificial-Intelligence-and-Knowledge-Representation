<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
<script type="text/x-mathjax-config"> MathJax.Hub.Config({ tex2jax: {inlineMath: [['$', '$']]}, messageStyle: "none" });</script>

# Meta-interpreters in Prolog

`A program fully developed in Prolog`.

## 1. Introduction 
The main difference between **meta-predicates** and **meta-interpreters** is: **meta-predicates** are built-in Prolog tools that influence the standard execution of the program. In contrast, **meta-interpreters** are programs, designed and tested by a developer, that take another program in input and define their own interpretation rules. 

Simply, we can develop a new interpreter that follows our **interpretation rules**.

The meta-interpreters use two main meta-predicate:

1. ...   
2. 