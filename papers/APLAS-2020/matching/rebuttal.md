## reviews and rebuttal

### Review #46A

Overall merit 3. Weak accept
Reviewer expertise 3. Knowledgeable

###### Paper summary

The paper presents an algorithm for synthesising code that implements pattern matching efficiently using relational programming in the miniKanren system. The design is elegant and I found the paper a pleasure to read. It begins with a concise formal specification of the problem at hand from which a naive relational implementation is constructed. Then this is refined via several optimisations to a more tractable implementation.

Where the paper is a little lacking is in the evaluation. The authors evaluate their own implementation on some toy examples, and establish that it works on small examples, but doesn't yet scale in practice. I'd like to see a comparison with a hand-coded bespoke implementation in order to get a better feel for how far away the relational approach is from being practical.

###### Comments for author

By including links to your code you have compromised the lightweight double-blind guidelines. In future you might consider submitting these as unanonymised auxiliary material that can be seen by referees after they've written their reviews. At the very least, please don't include GitHub URLs with your name in them!

I enjoyed this paper. I think it tackles an important and interesting problem. However, I was left feeling that I want a bit more, or at least more discussion of the alternative approaches to the problem. You should at least have a control to benchmark against. Have you considered other approaches to pattern matching synthesis? Have you considered stochastic methods, for instance, for obtaining (good enough) results more quickly?

###### Minor comments:

There are quite a few grammatical minor errors in the writing, but none that made it hard to read. I've only documented a few of them.

Inference rules: I think it's clearer if you render a horizontal line for each inference rule even if the premise is empty (vacuously true)

You might consider using a different notation from -->\_ as the may be mistaken for a Kleene star.

Page 6: I prefer to see alternatives denoted by | rather than just new lines.

Page 7: "steps is details"

Page 8: "from relational point of view due to" should be "from a relational point of view for"

Page 8: "with an infinite number"

Page 8: "of a complete sample set"

Page 9: "From a relational point of view"

Page 11: "they behavior"

Page 13: What are absent^o and symbol^o (it'd be nice to have a short sentence summarising their purpose)

Page 13: Is 3^2 supposed to be 2^3?

Page 13: "extensions also does not belong to" should be "extensions also do not belong to"

Page 16: "on a very small problems" should be "on very small problems"

### Review #46B

Overall merit 3. Weak accept

Reviewer expertise 2. Some familiarity

###### Paper summary

This paper describes a method for synthesizing pattern matching programs. The main use case would seem to be in compiling functional languages. The method uses an encoding of pattern matching as a relational programming problem, then hands the problem off to a relational programming solver to do the synthesis. The paper is fairly clearly written (although there are quite a few typos), the problem seems relevant and the approach to solve it seems nice. The paper includes some experimental results, but these are somewhat limited (there are 8 problems that the tool can solve and 1 that it cannot).

###### Comments for author

The majority of the paper describes the encoding of pattern matching synthesis as relational programming, with a good chunk of the paper giving some helpful background on what relational programming is. There are some difficulties involved in the encoding due to limitations in the relational programming language (MiniKanren) they use to specify the problem, but the authors show how to modify their encoding in order to meet these limitations.

The main technical questions I was left with relate to the generation of the "complete set of samples". The authors say that, in order to work around a lack of universal quantifiers in MiniKanren, they generate a set of concrete inputs for the synthesis procedure that is exponential in the depth of the synthesis problem (which itself is linear in the size of the pattern matching expression). They then describe an optimization which reduces the size of this set. My questions are:

- Is the size of the sample set still exponential after their optimization? (I couldn't figure this out directly from the definition, or from the experimental results table).

- How important is the size of this set, and in particular is the size of this set the thing that prevents the example in Figure 6 from going through? It seems like an optimally small sample set would be relatively straightforward to compute by hand (although I might be wrong about that), so it would be interesting to know whether the example in Figure 6 goes through with a manually constructed sample set or not.

- In section 2 (related work), you say that constructing a minimal decision tree is NP-hard, but the approach you're proposing seems to be at least exponential time (and space) due to explicitly enumerating an exponentially large sample set. How does your approach compare experimentally to just minimizing decision trees?

As stated above, the experimental section is a little weak and in particular doesn't include a comparison to other tools (the authors say that they have taken their benchmark set largely from the literature, so it seems odd to not compare to the tools the benchmarks were taken from).

Regarding the typos, there are quite a few (although they're mostly minor), so the authors should go through and proof read once more before final submission. Some examples:

- Abstract -- "We present a completely declarative approach to synthesizing pattern matching construct implementation based on application of relational programming" -> "...synthesizing pattern matching construct implementations based on the application of relational programming".

- Section 1 -- "On an initial evaluation ... showed our synthesizer performing well" -> "An initial evaluation ... showed our synthesizer performing well".

- Section 2 -- "...minimizing the size of decision tree -> "...minimizing the size of a decision tree".

- Section 4 -- "We first describe the general idea, and then consider these steps is details" -> "...and then consider the steps in detail".

- Section 5 -- "if we can detect ... , and prohibit" -> "if we can detect ... , we can prohibit"

### Review #46C

Overall merit 3. Weak accept

Reviewer expertise 2. Some familiarity

###### Paper summary

This paper proposes a new approach to synthesize pattern matching expressions to optimize program sizes using relational programming. The approach constructs a relation between matching expressions and switch programs to check the validity of synthesized switch programs and uses miniKanren to search for an optimized program. The relational interpreter has a key role to make the relation of semantic equivalence between two languages. The authors show that it is capable to synthesize switch programs for a number of benchmarks.

###### Comments for author

Strengths:

- Novelty: Demonstrate the feasibility of a relational interpreter approach for optimizing pattern matching expressions

- Presentation: Well-written, explanations easy to follow

Weaknesses:

- Significance: Small improvement over the existing approach to the optimal solution

- Scalability: Does not work for large expressions

In the Related work section, the authors describe other approaches as "which require a separate customized algorithms", but it is unclear how this approach can implement for various constructs in a unified way.

The Future work section states that "It looks technically easy to extend ..." but it lacks explanation and justification. Because the present work works only for simple pattern matching, it does not seem to be appropriate to discuss other solutions for specific forms. It would be better to develop this approach to handle other extensions in a uniform way.

In the Optimization section, it seemed that these optimizations would become more complex to cover other extensions. It could be another issue whether these optimizations are general enough when other extensions are handled if the authors try to argue this approach can handle extensions.

In the Evaluation section, it is better to show the difference of code size between the first answer and the last optimal answer in the table. The authors show only one example (the 4th benchmark), but it reduces six switch expressions to four switch expressions. Thus, it is difficult to decide how much improvement is made with only one example.

### Rebuttal

##### #46A

> Have you considered stochastic methods, .... for obtaining (good enough) results more quickly?

Not yet.

##### #46B

> Is the size of the sample set still exponential after their optimization?

> How important is the size of this set, and in particular is the size of this set the thing that prevents the example in Figure 6 from going through? It seems like an optimally small sample set would be relatively straightforward to compute by hand (although I might be wrong about that), so it would be interesting to know whether the example in Figure 6 goes through with a manually constructed sample set or not.

The sample set is exponential in general, so there are cases when described optimization is not applicable. For example, we can enumerate all binary trees to certain depth and convert each inhabitant into pattern, using wildcard only for elements of the tree. We will get 2^depth patterns and so many examples.

But in many cases decribed optimization can seriously reduce examples set. The real amount of examples highly depends on a number of constructors in scrutinee's type an on wildcards placement in patterns. We will add type information to the paper for PCF example to show significance of complexity of scrutinee's type.

Last benchmark is reduced version of the one from Figure 6 not only by patterns, but also by amount of constructors in scrutinee's type. It requires 20 examples for synthesis. If we reuse types from benchmark on Figure 6 then we will generate 364 examples. The full example from figure 6 will require 11102 examples. But if we change `Val (Int _)::_` to `(Val _)::_` in the 3rd clause then depth of the second component of scrutinee will be less and count of examples will be reduced to 2366. All numbers with described optimization enabled.

> ..., you say that constructing a minimal decision tree is NP-hard, but the approach you're proposing seems to be at least exponential time (and space) due to explicitly enumerating an exponentially large sample set. How does your approach compare experimentally to just minimizing decision trees?

> ..., the experimental section is a little weak and in particular doesn't include a comparison to other tools (the authors say that they have taken their benchmark set largely from the literature, so it seems odd to not compare to the tools the benchmarks were taken from).

Our goal is to 1) speedup synthesis enough to deal with PCF example; and 2) embed our approach into OCaml compiler. In the current state it seems too early to compare speed of synthesis to the state-of-art methods like the ones described in L.Maranget's papers. There approach from 2008 paper demostraints significant result only on PCF example (as said in the end of chapter 9 of 2008 paper).

##### #46C

> In the Evaluation section, it is better to show the difference of code size between the first answer and the last optimal answer in the table.

We will do it in the next version of the paper.
