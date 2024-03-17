# CS3110-Final-Project

Team Name: Svv Juniors

Team Member Names:

- Janna Lin (jnl77)
- Bokai Liu (bl752)
- Rachel Yan (sy625)

---
## Table of Contents
1. [Project Introduction](#intro)
2. [Linear Temporal Logic (LTL) Primer](#ltl)
3. [Büchi automaton Primer](#buchi)
4. [Construction of Büchi automaton from LTL](#conv)
5. [Reference](#ref)
   
## <a name = "intro">Project Introduction</a>
In a world where systems are getting a tad complex, having a way to check if they behave as expected is kinda handy. Amidst this, having a neat tool that can translate everyday language into something a machine understands is pretty cool. That's where our little project, the Linear Temporal Language (LTL) to Büchi Automata converter, comes into play.

We aim to build a Flat LTL to Buchi Automata converter in this project. This converter takes in strictly-formatted human language, parses and synthesizes it into a simplified Linear Temporal Logic expression, and converts it into a reliable Büchi automaton that is visualized and verifiable. With the automaton being generated, the user is able to verify if their subsequent tentative actions is executable. 

Not specific to this project, a pragmatic implementation can be in autonomous vehicle development. Our converter could translate human-readable safety specifications into Büchi automata, enabling developers to verify whether proposed driving algorithms adhere to critical safety norms before real-world deployment. To ensure that the dependencies are installed to support our project, please refer to the [installation](INSTALL.md) guide for further guidance.

## <a name = "ltl">Linear Temporal Logic (LTL) Primer</a>

### 1. What is LTL?

Linear Temporal Logic (LTL) is a modal logic that allows reasoning about the ordering of events over time. In computer science, LTL is often used for specifying and verifying properties of hardware and software systems.

### 2. Syntax of LTL

Let `AP` be a set of atomic propositions where any element `p` $\in$ ` AP` is a Boolean variable.

LTL formulas are constructed from atomic propositions and the following logical and temporal operators:


Where:
- `¬` represents negation (logical operator).
- `∨` represents disjunction (logical operator).
- `∧` represents conjunction (logical operator).
- `=>` represents implies (logical operator).
- `<=>` represents equivalent (logical operator).
- `G` or `□` represents always (temporal operator).
- `F` or `◇` represents eventually (temporal operator).
- `X` represents next (temporal operator).
- `U` represents until (temporal operator).
- `W` represents weak until (temporal operator).
- `R` represents release (temporal operator).

---

## 3. Semantics of LTL

Given a model `M` and a path `π` in `M`, the satisfaction of an LTL formula over a path is defined as follows:

- `π |= p` if `p` is true at the first position of `π`.
- `π |= ¬ϕ` if `π` does not satisfy `ϕ`.
- `π |= ϕ1 ∨ ϕ2` if `π` satisfies `ϕ1` or `ϕ2`.
- `π |= ϕ1 ∧ ϕ2` if `π` satisfies `ϕ1` and `ϕ2`.
- `π |= ϕ1 → ϕ2` if either `π` does not satisfy `ϕ1` or `π` satisfies `ϕ2`.
- `π |= ϕ1 ↔ ϕ2` if `π` satisfies `ϕ1` if and only if `π` satisfies `ϕ2`.
- `π |= ϕ1 □ ϕ2` if for all positions `i` in `π`, if `π[i]` satisfies `ϕ1` then `π[i+1]` satisfies `ϕ2`.
- `π |= ϕ1 ◇ ϕ2` if there exists a position `i` in `π` such that `π[i]` satisfies `ϕ1` and `π[i+1]` satisfies `ϕ2`.
- `π |= X ϕ` if the next state in `π` satisfies `ϕ`.
- `π |= ϕ1 U ϕ2` if there exists a position `i` in `π` such that `π[i]` satisfies `ϕ2` and for all `j < i`, `π[j]` satisfies `ϕ1`.
- `π |= ϕ1 W ϕ2` if either `π` satisfies `ϕ1 U ϕ2` or for all positions in `π`, `π[i]` satisfies `ϕ1`.
- `π |= ϕ1 R ϕ2` if for every position `i` in `π`, if `π[i]` satisfies `ϕ1` then there exists a `j ≥ i` such that `π[j]` satisfies `ϕ2`.
  
---
## 4. Flat LTL Logic
Flat LTL is a subset of normal LTL. This fragment can be translated into Büchi automaton in linear size. The set of Flat LTL formula `L` is given in the following manner:
`ϕ := θ | θ U ϕ | ϕ R θ | ¬Δ | ϕ1 ∨ ϕ2 | ϕ1 ∧ ϕ2`, where `θ` is a propositional formula defined by: `θ := true | p | θ1 ∧ θ2 | ¬θ `, and where `Δ` is a temporal formula where `Δ := Δ U θ | θ R Δ | ¬Δ`.

We can convert a flat LTL to the form Flat Positive Normal form (FPNF), with the fact that "negations only occur adjacent to atomic propositions."


## <a name = "buchi">Büchi automaton Primer</a>

### What is a Büchi automaton?

A Büchi automaton is a type of ω-automaton (omega-automaton) used to recognize infinite words. It's often employed in formal verification, specifically in the area of model checking for linear temporal logic (LTL) properties.

### Structure of Büchi automaton

A Büchi automaton is similar to a nondeterministic finite automaton (NFA) but operates on infinite words. It is defined as a tuple `B = (Q, Σ, δ, q0, F)`:


Where:
- `Q` is a finite set of states.
- `Σ` is the input alphabet (finite set of symbols).
- `δ` is a transition relation `δ ⊆ Q × Σ × Q`.
- `q0` is the initial state.
- `F` is a set of accepting states.

### Acceptance Condition

A run of a Büchi automaton on an infinite word is accepting if it visits one of the accepting states infinitely often. In other words, a Büchi automaton accepts an infinite word if there exists a run of the automaton on that word which enters some state from `F` an infinite number of times.

### Usage in LTL Model Checking

The Büchi automaton plays a significant role in LTL model checking. Given an LTL formula, one can construct a Büchi automaton that recognizes all models of the formula. With this automaton, one can then check if a system satisfies the given LTL property by analyzing the intersection of the system's behavior with the Büchi automaton.

## <a name = "conv"> Construction of Büchi automaton from LTL </a>
The main logic we follows in our project is by the algorithm proposed in Kanso and Kansou's paper. Since we omit the transition of "Next" in our implementation, we mainly focus on two situations:
- If the input is a propositional formula, the corresponding automaton only includes two states (`s0, s1`) and two transition (`tr1, tr2`). `s0` refers to the initial state, and `s1` refers to the final state. The condition recorded in `tr1` is just the input propositional formula, and `tr2` is a self-transition at `s1` that accepts any input. 
- If the input is not a propositional formula, we may convert any other temporal operator in our Flat LTL to one that is represented by `U`, and follows the algorithm denoted by the paper to convert.

## <a name = "ref"> Reference </a>
Kanso, B., & Kansou, A. (2019). Converting a Subset of LTL Formula to Buchi Automata. 
  International Journal of Software Engineering & Applications, 10(2), [Page Range]. 
  https://doi.org/10.5121/ijsea.2019.10204


