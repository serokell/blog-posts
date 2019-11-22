## Incomplete and Utter Introduction to Modal Logic, Part 1.

TODO: InnerPreview.jpeg

### Foreword

Modal logic is one of the most popular branches of mathematical logic. Modal logic covers such areas of human knowledge as mathematics (especially, topology and graph theory), computer science, linguistics, artificial intelligence, and philosophy. Moreover, modal logic in itself still attracts by its mathematical beauty. We would like to introduce the reader to modal logic, fundamental technical tools, and connections with other disciplines. Our introduction is not so comprehensive indeed, the title is an allusion to [the wonderful book by Stephen Fry](http://www.stephenfry.com/store_products/incomplete-utter-history-classical-music/).

### History and background

The grandfather of modal logic is the 17th-century German mathematician and philosopher Gottfried Leibniz, one of the founders of calculus and mechanics.

TODO: Leibniz.jpeg

From his point of view, there are two kinds of truth. The statement is called necessarily true if it's true for any state of affair. Note that, one also has possibly true statements. That is, such statements that could be false, other things being equal. For instance, the proposition "sir Winston Churchill was a prime minister of Great Britain in 1940" is possibly true. As we know, [Edward Wood, 1st Earl of Halifax](https://en.wikipedia.org/wiki/Edward_Wood,_1st_Earl_of_Halifax), also could be a prime minister at the same time. Of course, one may provide any other example from the history of the United Kingdom and all of them would be valid to the same degree.
In other words, any factual statement might be possibly true since "the trueness" of any factual statement depends strongly on circumstances and external factors. The example of a statement which is necessarily true is $1 + 1 = 2$, where signs $1$, $2$, $+$ and $=$ are understood in the usual sense.

Modal logic became a part of contemporary logic at the beginning of the 20th century. In the 1910s, Clarence Lewis, American philosopher, was the first who proposed to extend the language of classical logic with two modal connectives $\Box$ and $\Diamond$.

TODO: Lewis.jpeg

Informally, one may read $\Box \varphi$ as $\varphi$ is necessary. $\Diamond \varphi$ has a dual interpretation as $\varphi$ is possible. Lewis sought to analyse the notion of necessity from a logical point of view. Thus, modalities in mathematical logic arose initially as a tool for philosophical issue analysis. Lewis understood those modalities intuitively rather than formally. In other words, modal operators had no mathematically valid semantical interpretation.

In the year 1944, Alfred Tarski and John McKinsey wrote a paper called [The Algebra of Topology](https://www.dimap.ufrn.br/~jmarcos/papers/AoT-McKinsey_Tarski.pdf). In this work, the strong connection between modal logic ${\bf S}4$ and topological and metric spaces was established. Historically, topological semantics is the very first mathematical semantics of modal logic. Those results marked the beginning of a separate branch of modal logic that studies the topological aspects of modalities.

TODO: Tarski.jpeg

In the 1950s and 1960s, Saul Kripke formulated relational semantics for modal logic. Note that we will mostly consider Kripke semantics in this post.

TODO: Kripke.jpeg

Later, in the 1970s, such mathematicians as Dov Gabbay, Krister Segerberg, Robert Goldblatt, S. K. Thomason, Henrik Sahlqvist, Johan van Benthem provided the set of technical tools to investigate systems of modal logic much more deeply and precisely. So, modal logic continues its development in the direction that was established by these researchers.

Let us briefly overview some areas of modal logic use:

1. _Topology and graphs_

Modal logic provides a compact and laconic language to characterise some properties of directed graphs and topological spaces. In this blog post, we study Kripke frames as the underlying structures. Without loss of generality, one may think of Kripke frames as directed graphs. It means that formal definitions of a Kripke frame and a directed graph are precisely the same. Here, modal language is a powerful tool in representing first-order adjacency properties. We realise the fact that not every reader has a background in first-order logic. In order to keep the post self-contained, we remind the required notions from first-order logic to describe this connection quite clearly.

As we will see, modal logic is strongly connected with binary relations through Kripke frames. The properties of binary relation in a Kripke frame are mostly first-order. We consider examples of first-order relation properties expressed in modal language. Moreover, we formulate and discuss the famous Salqvist's theorem that connects modal formulas of the special kind with binary relation properties that are first-order definable. Anyway, in such a perspective, one may consider modal logic as a logic of directed graphs since there is no formal difference between a binary relation and edges in a graph. That is, one may use modal logic in directed graph characterisation in a limited way.

You may draw any directed graph you prefer on a plane. A graph is a combinatorial object rather than a geometrical one. A topological space is closer to geometry. Although, there's a topological way of graph consideration, however. We also discuss how exactly modal logic has a topological interpretation. In the author's opinion, topological and geometrical aspects of modal logic are one of the most astonishing and beautiful.

It is a well-known result proved by Alfred Tarski and John McKinsey that the modal logic ${\bf S}4$ is the logic of all topological spaces. Similarly to directed graphs, modal logic might be used in classifying topological spaces and other spatial constructions in a restricted way. We will discuss the topological and spatial aspects of modal logic in the second part.

2. _The foundations of mathematics_

The foundation of mathematics is the branch of mathematical logic that studies miscellaneous metamathematical issues. In mathematics, we often consider some abstract structure and formulate a theory based on some list axioms that describe primitive properties of considered constructions. The examples are group theory, elementary geometry, graph theory, arithmetics, etc.

In metamathematics, we are interested in what a mathematical theory is in itself. That is, metamathematics arose at the beginning of the previous century to answer philosophical questions mathematically. The main interest was to prove the consistency of formal arithmetics with purely formal methods. As it's well known, Gödel's famous incompleteness theorems place limits the formal proof of arithmetics consistency within arithmetics itself.

However, modal logic also allows one to study the properties of provability in formal arithmetical theories. Moreover, the second Gödel's incompleteness theorem has a purely modal formulation! We discuss that topic in more detail in the follow-up of the series.

### Basic definitions and constructions

#### Modal language

The first is this to define a modal language. That's the starting point for every logic which we take into consideration. In our case, a modal language is a grammar according to rules of which we write logical formulas enriched with modalities. Let $\operatorname{PV} = \{ p_0, p_1, \dots \}$ be a countably infinite set of propositional variables, or atomic propositions, or propositional letters. Informally, a propositional variable ranges over such atomic statements as "it is raining" or something like that.

A modal language as the set of modal formulas $\operatorname{Fm}$ is defined as follows:

1. Any $p_i \in \operatorname{PV}$ is a formula.
2. If $\varphi$ is a formula, then $\neg \varphi$ is a formula.
3. If $\varphi$ and $\psi$ are formulas, then $(\varphi \circ \psi)$ is formula, where $\circ \in \{ \to, \land, \lor\}$.
4. If $\varphi$ is a formula, then $\Box \varphi$ is a formula.

Note that the possibility operator $\Diamond$ is expressed as $\Diamond =_{def} \neg \Box \neg$. In our approach, the statement $\varphi$ is possibly true iff it's false that necessiation of $\neg \varphi$ is true. Also, one may introduce $\Diamond$ as the primitive one, that is $\Box =_{def} \neg \Diamond \neg$.

As we said, a modal language extends the classical one. One may ask how we can read $\Box$. Here are several ways to understand this modal connective:
1. $\Box \varphi$ as $\varphi$ is necessary. This is historically the very first way of the modal connective reading. Dually, one may read $\Diamond \varphi$ as $\varphi$ is possible. Modalities of this kind are alethic modalities.
2. $\Box \varphi$ as it is known that $\varphi$. In this case, we call $\Box$ an epistemic modality.
3. $\Box \varphi$ as $\varphi$ is a statement provable in formal arithmetics.
4. $\Box$ as an interior in a topological space. Here, we call such modality a topological one.
5. $\Box \varphi$ as there will be $\varphi$ always in the future, a temporal modality, in other words.

We will discuss items 3-5 in the next part of the series as promised.

#### Modal logic

We defined the syntax of modal logic above. But syntax doesn't provide logic, only grammar. In logic, one has inference rules and the definition of proof. We also want to know what kind of modal statements we need to prove basically. Firstly, let us describe a more basic notion like normal modal logic.

By normal modal logic, we mean a set of modal formulas $\mathcal{L}$ that contains all Boolean tautologies; ${\bf AK}$-axiom (named after Kripke) $\Box (p \to q) \to (\Box p \to \Box q)$ and closed under the following three inference rules:

1. If $\varphi \in \mathcal{L}$ and $\varphi \to \psi \in \mathcal{L}$, then $\varphi \in \mathcal{L}$ (Modus ponens). That is the basic logical rule that allows you to get a consequence from an implication if you have a premise.
2. If $\varphi \in \mathcal{L}$, then $\Box \varphi \in \mathcal{L}$ (Necessiation rule). If $\varphi$ belongs to a logic, then $\Box \varphi$ belongs to $\mathcal{L}$ too. Informally, if $\varphi$ is provable, then $\varphi$ is necessary.
3. If $\varphi(p) \in \mathcal{L}$, then $\varphi (p := \psi) \in \mathcal{L}$. If formula $\varphi$ with variable $p$ belongs to a logic, then its instances belong to the same logic. Here, the instance of the formula is the result of a substitution of another formula instead of a variable.
In other words, any normal modal logic is _closed_ under substitution.

The fact that any normal modal logic contains all Boolean tautologies tell us that modal logic extends the classical one. Literally, the Kripke axiom ${\bf AK}$-axiom denotes that $\Box$ distributes over implication. Such an explanation doesn't help us so much, indeed. It's quite convenient to read this axiom in terms of necessity. Then, if the implication $\varphi \to \psi$ is necessary and the premise is necessary, then the consequence is also necessary. For instance, it is necessary that if the number is divided by four then this number is divided by two. Then, if it is necessary that the number is divided by four, then it is necessary that it's divided by two.

Of course, within the natural language that we use in everyday speech the last two sentences sound quite monotonous, but we merely illustrated how this logical form works by example. In such a situation, it's crucially important to follow the formal structure even if this structure looks wordy and counterintuitive. Of course, this sometimes runs counter to our preferences in linguistic aesthetics, although structure following allows analysing modal sentences much more precisely.

If we take into consideration some logical system represented by axioms and inference rules, then one needs to determine what derivation is in an arbitrary normal modal logic. A derivation of formula $\varphi$ in normal modal logic $\mathcal{L}$ is a sequence of formulas, each element of which is either axiom or some formula obtained from the previous ones via inference rules. The last element of such a sequence is a formula $\varphi$ itself.

Here is an example of derivation in normal modal logic:

1. $\varphi \to (\psi \to (\varphi \land \psi))$
       Boolean tautology
2. $\Box (\varphi \to (\psi \to (\varphi \land \psi)))$
       Necessiation rule
3. $\Box (\varphi \to (\psi \to (\varphi \land \psi))) \to (\Box \varphi \to \Box (\psi \to (\varphi \land \psi)))$
       ${\bf AK}$-axiom.
4. $\Box \varphi \to \Box (\psi \to (\varphi \land \psi))$
       Modus ponens, (2), (3)
5. $\Box (\psi \to (\varphi \land \psi)) \to (\Box \psi \to \Box (\varphi \land \psi))$
       ${\bf K}$-axiom
6. $\Box \varphi \to (\Box \psi \to \Box (\varphi \land \psi))$
       Transitivity of implication, (4), (5)
7. $(\Box \varphi \to (\Box \psi \to \Box (\varphi \land \psi))) \to (\Box \varphi \land \Box \psi \to \Box (\varphi \land \psi))$
       The instance of a Boolean tautology
8. $(\Box \varphi \land \Box \psi) \to \Box (\varphi \land \psi)$
       Modus ponens, (6), (7)

We leave the converse implication as an exercise to the reader.

The minimal normal modal logic ${\bf K}$ is defined via the following list of axioms and inference rules:
1. $(p \to (q \to r)) \to ((p \to q) \to (p \to q))$
2. $p \to (q \to p)$
3. $p \to (q \to (p \land q))$
4. $(p_1 \land p_2) \to p_i$, $i = 1,2$
5. $p_i \to (p_1 \lor p_2)$, $i = 1,2$
6. $(q \to p) \to ((r \to p) \to (q \lor r) \to p)$
7. $(p \to q) \to ((p \to \neg q) \to \neg p)$
8. $\neg \neg p \to p$
9. $\Box (p \to q) \to (\Box p \to \Box q)$
10. Inference rules: Modus Ponens, Necessiation, Substitution.

The axioms (1)-(8) are exactly axioms of the usual classical propositional logic that axiomatise the set of Boolean tautologies together with Modus Ponens and Substitution rule. The axiom (9) is a Kripke axiom ${\bf AK}$. One may also claim that ${\bf K}$ is the smallest normal modal logic. In the definition of normal modal logic, we just require that the set of formulas should contain Boolean tautologies, etc. By the way, there might be something different from the required list of formulas. For instance, the set of all formulas is also a normal modal logic, the trivial one. The minimal normal modal logic is a normal modal logic that contains only Boolean tautologies, ${\bf AK}$-axiom and closed under the inference rules, no less no more. The logic ${\bf K}$ is the underlying logic for us. Other modal logics are solely extensions of ${\bf K}$. Note that modal logic is not needed to be a normal one. Weaker modal logics are studied via so-called [neighbourhood semantics](https://www-cs.stanford.edu/~epacuit/classes/esslli/nbhdesslli.pdf) that we drop in our introductory post.

#### Kripke semantics

We have defined the syntax of the modal logic above introducing the grammar of a modal language. Let us define Kripke frames and models to have the truth definition in modal logic.

__Definition__
A Kripke frame is a pair $\mathbb{F} = \langle W, R \rangle$, where $W$ is a non-empty set, a set of so-called _possible worlds_, informally. Note that we will call an element of $W$ a world or a point. World and point are synonyms for us. $R \subseteq W \times W$ is a binary relation on $W$. For example, the set $\{0, 1, ..., n\}$ with strict order relation $<$ is a Kripke frame. Moreover, any directed graph might be considered as a Kripke frame, where the set of vertices is a set of possible worlds and the set of edges is a binary relation.

__Definition__
A Kripke model is a pair $\mathcal{M} = \langle \mathcal{F}, \vartheta \rangle$, where $\mathbb{F} = \langle W, R \rangle$ is a Kripke frame and $\vartheta : \operatorname{PV} \to 2^W$ is a valuation function that maps any proposition variable to some subset of possible words. Informally, we should match any atomic statement with corresponding states of affairs in which this atomic statement is true. For complex formulas, we introduce the truth definition inductively. We denote this relation as $\mathcal{M}, w \models \varphi$ that should be readen as "in model $\mathcal{M}$, in world $w$ the statement $\varphi$ is true".

1. $\mathcal{M}, w \models p$ iff $w \in v(p)$
2. $\mathcal{M}, w \not\models \bot$, that is, there is no point in which false could be true.
3. $\mathcal{M}, w \models \varphi \to \psi$ iff $\mathcal{M}, w \models \varphi$ implies $\mathcal{M}, w \models \psi$
4. $\mathcal{M}, w \models \Box \varphi$ iff for all $v$ such that $w R v$, $\mathcal{M}, v \models \varphi$

It is not so difficult to obtain the truth conditions for other connectives. One needs to keep in mind that the other Boolean connectives are introduced via implication and bottom as
1. $\neg \varphi \leftrightharpoons \varphi \to \bot$
2. $\varphi \lor \psi \leftrightharpoons \neg (\varphi \to \neg \psi)$
3. $\varphi \land \psi \leftrightharpoons \neg (\neg \varphi \lor \neg \psi)$

We also know that $\Diamond =_{def} \neg \Box \neg$, so the truth definition for $\Diamond \varphi$ is the following one:

$\mathcal{M}, w \models \Diamond \varphi$ iff there exists $v$ such that $w R v$ and $\mathcal{M}, v \models \varphi$.

There are a lot of examples of Kripke models, indeed. Here, we refer the reader to the book [Modal Logic of Open Minds by Johan van Benthem](http://fenrong.net/teaching/mljvb.pdf) to study miscellaneous cases in depth. Let us consider briefly the following graph as a Kripke frame with the valuation map $\vartheta$:

TODO: ExampleFrame.jpeg
TODO: ExampleModel.jpeg

As an exercise, the reader might determine which formulas are true in every point of the model above.

Also, we will use the following notions:
1. $\mathcal{M} \models \varphi$ iff $\mathcal{M}, w \models \varphi$ for each $w$. That is, a formula $\varphi$ is true in a Kripke model $\mathcal{M}$ if and only if it's true in each possible world $w$
2. $\mathcal{F} \models \varphi$ iff for each valuation $v$ and for all $w$, $\langle \mathcal{F}, w \rangle \models \varphi$.
One should read $\mathcal{F} \models \varphi$ as $\varphi$ is valid in a frame $\mathcal{F}$ In other words, a formula $\varphi$ is true in a Kripke frame $\mathcal{F}$ if and only if it's true in every Kripke model on this frame as an underlying one.
3. $\operatorname{Log}(\mathcal{F}) = \{ \varphi \in Fm \: | \: \mathcal{F} \models \varphi \}$, where $\mathcal{F}$ is a Kripke frame. We will call the set $Log (\mathcal{F})$ the logic of frame $\mathcal{F}$
4. Let $\mathbb{F}$ be a class of Kripke frames, then $\operatorname{Log}(\mathbb{F}) = \bigcap \limits_{\mathcal{F} \in \mathbb{F}} \operatorname{Log}(\mathcal{F})$. More simply, the logic of class is a set of formulas each of which is true in every Kripke frame that belongs to this class.

In logic, we often require that any true formula should be provable. If some logic satisfies this requirement, then we call that the logic is _complete_. Here, a modal logic $\mathcal{L}$ is a Kripke _complete_ if and only if there exists some class of Kripke frames $\mathbb{F}$ such that $\mathcal{L} = \operatorname{Log}(\mathbb{F})$. That is, any valid formula in this class of frames is provable in $\mathcal{L}$.

So, we're going to solve the following issue. What's the most general logic which is valid in an arbitrary Kripke frame? In other words, we are going to characterise the logic of all possible Kripke frames. We defined above what minimal normal modal logic is. One may show that this logic is the logic of all Kripke frames:

__Theorem 1__
${\bf K} = Log (\mathbb{F})$, where $\mathbb{F}$ is the class of all Kripke frames.

The left inclusion ${\bf K} \subseteq ML (\mathcal{F})$ called soundness is more or less obvious. The soundness theorem claims that the logic of all Kripke frames is the normal modal one. We will prove only that any formula provable in ${\bf K}$ is valid in any Kripke frame and show that $\operatorname{Log}(\mathcal{F})$ is closed under substitution, where $\mathcal{F} \in \mathbb{F}$ is an arbitrary Kripke frame.

Let us show that the normality axiom is valid in an arbitrary Kripke frame.

Let $\mathbb{F} = \langle W, R \rangle$ be a Kripke frame and $v : \operatorname{PV} \to \mathcal{P}(W)$ a valuation map. So, we have a Kripke model $\mathcal{M}  = \langle \mathbb{F}, v \rangle$. Let $\mathcal{M}, a \models \Box (\varphi \to \psi)$ and $\mathcal{M}, a \models \Box \varphi$. Let $a R b$, then $\mathcal{M}, b \models \varphi \to \psi$ and $\mathcal{M}, b \models \varphi$ by the truth definition for $\Box$. So $\mathcal{M}, b \models \varphi$, so far as Modus Ponens holds in every Kripke model. Thus, $\mathcal{M}, a \models \Box \varphi$.

Now we show that $\operatorname{Log}(\mathbb{F})$ is closed under substitution. We provide only a sketch since the full proof is quite technical.

Let $v : \operatorname{PV} \to \mathcal{P}(W)$ be a valuation and $\mathcal{M} = \langle \mathcal{F}, v \rangle$ a Kripke model. Let us put $||\varphi|| = \{ w \in W \: | \: \mathcal{M}, w \models \varphi \}$, the set of all points in a Kripke model, where the formula $\varphi$ is true. Let $\mathcal{F} \models \varphi(p)$ and let $\psi$ be an arbitrary formula. We build a Kripke model $\mathcal{M} = \langle \mathcal{F}, v' \rangle$ such that $v'(p) = ||\psi||$. Then, one may show by induction that $\mathcal{M}, x \models \varphi [p := \psi] \Leftrightarrow \mathcal{M}', x \models \varphi(p)$.

The right inclusion (${\bf K} \supseteq \operatorname{Log}(\mathbb{F})$) called completeness is quite non-trivial, one needs to build the so-called canonical model. We haven't defined what a canonical frame and model are. The idea of a canonical frame is that an observed logic itself forms a Kripke frame (and Kripke model too). Canonical frames and models often allow us to prove the fact that some normal modal logic is complete with respect to some class of Kripke frames. We provide only the main proof sketch. The following construction is very and very abstract, but sometimes one has to be patient to produce fruits.

Let $\Gamma$ be a set of formulas and $\mathcal{L}$ a normal modal logic. $\Gamma$ is $\mathcal{L}$-_inconsistent_, if there exist some formulas $\varphi_1, \dots, \varphi_n$ such that $\neg (\varphi_1 \land \dots \land \varphi_n) \in \mathcal{L}$. That is, the set of formulas $\Gamma$ is $\mathcal{L}$-inconsistent, if there exists a finite subset such that negation of its conjuction is provable in an observed logic. $\Gamma$ is $\mathcal{L}$-_consistent_, if $\Gamma$ is not inconsistent.

A $\mathcal{L}$-consistent set $\Gamma$ is maximal if it doesn't have any non-trivial extensions. In other words, if $\Gamma$ is a subset of $\Gamma'$, where $\Gamma'$ is a $\mathcal{L}$-consistent, then $\Gamma = \Gamma'$.

Now we are ready to define a canonical frame. Let $\mathcal{L}$ be a normal modal logic, then a canonical frame is a pair $\mathcal{F}_{\mathcal{L}} = \langle W_{\mathcal{L}}, R_{\mathcal{L}} \rangle$, where $W_{\mathcal{L}}$ is the set of all maximal $\mathcal{L}$-consistent set. $R$ is a canonical relation such that:

$\Gamma R_{\mathcal{L}} \Delta \Leftrightarrow \Box \varphi \in \Gamma \Rightarrow \varphi \in \Delta$. That is, any boxed formula from $\Gamma$ can be unboxed in $\Delta$.

A canonical model is a canonical frame equipped with the canonical valuation $\vartheta_{\mathcal{L}}(p) = \{ \Gamma \in W_{\mathcal{L}} \: | \: p \in \Gamma \}$. This valuation maps each variable to set of all maximal $\mathcal{L}$-consistent sets that contain a given variable.

Here comes the theorem:
__Theorem__
1. $\mathcal{M}_{\mathcal{L}}, \Gamma \models \varphi \Leftrightarrow \varphi \in \Gamma$,
i.e., the truth definition is generated by membership to some maximal consistent set.

2. $\varphi$ is provable in $\mathcal{L}$ iff $\mathcal{M} \models \varphi$,
i.e. any formula is provable iff it is true at every point of a canonical model.

3. If $\varphi \in \operatorname{Log}(\mathcal{F}_{\mathcal{L}})$, then $\varphi$ is provable in $\mathcal{L}$,
i.e. any provable formula is valid in a canonical frame.

The completeness theorem for ${\bf K}$ is a simple corollary from the theorem above. Any formula that provable in ${\bf K}$ is valid in every frame. If it is valid in every frame, then it is also valid in the canonical frame. Thus, a formula provable in ${\bf K}$. That's it. The construction above allow us to claim that any formula that valid in all possible Kripke frames is provable in ${\bf K}$. The reader might read the complete proof [here](https://www.doc.ic.ac.uk/~mjs/teaching/ModalTemporal499/CanonicalNormal_499_v0809_2up.pdf).

#### Modal logic meet first-order logic

As we told above, modal logic is strictly connected with first-order logic. First-order logic extends classical logic with quantifiers as follows. Classical propositional logic deals with statements and the ways of their combinations with such connectives as a conjunction, disjunction, negation, and implication. For example, if Neil Robertson will make [a maximum 147 break](https://www.youtube.com/watch?v=6cAagJs18IM), then he will win the current frame (a snooker frame, not Kripke frame). This statement has the form $p \to q$. $p$ denotes Neil Robertson will make a maximum 147 break. $q$ denotes he will win the current frame. In first-order logic, one may analyse formally universal and existential statements which tell about properties of objects.

More strictly, we add to the list of logical connectives quantifiers $\forall$ (for all ...) and $\exists$ (there exists ...). We extended the set of connectives, so one needs to redefine the notion of formula. Suppose also we have a countably infinite set of relation symbols (or letters) of arbitrary finite arity. We well denote them as $P$. Also one has a countably infinite set of individual variables $x, y, z, \dots$.

The notion of formula with such set of predicate symbols:

1. If $x_1, \dots, x_n$ are variables and $P$ is a relation symbol of an arity $n$, then $P(x_1, \dots, x_n)$ is a formula
2. $\bot$ is a formula
3. If $A, B$ are formulas, then $(A \to B)$ is a formula
4. If $x$ is a variable and $\forall x \: A$ is a formula

The other connectives and quantifiers are expressed as follows:

1. $\neg A := A \to \bot$
2. $(A \land B) := \neg (A \to \neg B)$
3. $(A \lor B) := \neg (\neg A \land \neg B)$
4. $(\exists x \: A) := \neg \forall x \: \neg A$

Note that there is a need to distinguish free and bound variables in a first-order formula. A variable is bounded if it's captured by a quantifier. Otherwise, a variable is called free. For instance, the variable $x$ is bounded in the formula $\exists x \: R(x, y, z)$. One may read this formula, for instance, as there is exists a point $x$ such that $x$ lies between points $y$ and $z$. Variables $y$ and $z$ are free in this formula since there are no quantifiers that bound them.

Now we would like to realise what truth for a first-order formula is. Unfortunately, we don't have truth tables with 0 and 1 as in classical propositional logic. The definition of truth for the first-order case is much more sophisticated. An interpretation of the first-order language (as the infinite set of relation symbols) is a pair $\langle A, I \rangle$, where $A$ is a non-empty set (a domain) and $I$ is an interpretation function. $I$ maps every relation letter $P$ of an arity $n$ to the function of type $A^{n} \to \{ 0, 1 \}$, i.e., some $n$-ary predicate on a domain $A$. To define truth conditions for first-order formulas, we suppose that we have an arbitrary variable assignment function $v$ such that $v$ maps every variable to some element of our domain $M$. An interpretation and variable assignment give us a first-order model, the definition of truth is the following one:

1. $\mathcal{M} \models_{\operatorname{FO}} P(x_1, \dots, x_n) \Leftrightarrow I(P)(v(x_1), \dots, v(x_n)) = 1$
2. $\mathcal{M} \models_{\operatorname{FO}} A \to B \Leftrightarrow \mathcal{M} \models_{\operatorname{FO}} A \Rightarrow \mathcal{M} \models_{\operatorname{FO}} B$
3. $\mathcal{M} \nvDash_{\operatorname{FO}} \bot$
4. $\mathcal{M} \models_{\operatorname{FO}} \forall x \: A(x) \Leftrightarrow \mathcal{M} \models_{\operatorname{FO}} A(a)$ for each $a \in A$

Note that the truth definition for existential formulas might be expressed as follows:
$\mathcal{M} \models \exists x \: A(x) \Leftrightarrow \mathcal{M} \models_{\operatorname{FO}} A(a)$ for some $a \in A$.

A first-order formula is satisfiable if it's true in some model and it's valid if it's true in every interpretation.

We use $\models_{\operatorname{FO}}$ in the same sense as in Kripke models, but with the index $\operatorname{FO}$ to distinguish both relations that denoted equally.

Let us comment on the conditions briefly. The first condition is the underlying one. As we said above any $n$-ary relation symbol maps to $n$-ary relation on observed domain, that is, $n$-ary truth function $A^n \to \{ 0,1 \}$. On the other hand, any variable (which should be free, indeed) maps to some element of a domain. After that, we apply the obtained truth function to the result of the variable assignment. We obviously require that an elementary formula $P(x_1, \dots, x_n)$ is true in the model $\mathcal{M}$ if the result of that application equals $1$. The last fact mean that a predicate is true on elements $v(x_1), \dots, v(x_n)$.

For example, suppose we have a ternary relation symbol $R$ such that we have interpret $R(x,y,z)$ as $y$ lies between points $x$ and $z$. Suppose also that our domain is the real line $\mathbb{R}$. We map our ternary symbol to the truth function $\pi$ such that $\pi(a,b,c) = 1$ if and only if $a \leq b$ and $b \leq c$. We also define the variable assignment as $v(x) = \sqrt{2}$, $v(y) = \sqrt{3}$, and $v(z) = \sqrt{5}$.

It is clear that in the model on real numbers $\mathcal{M}$ the formula $R(x,y,z)$ is true since $I(R)(v(x), v(y), v(z)) = \pi(\sqrt{2}, \sqrt{3}, \sqrt{5}) = 1$. The last equation follows from the obvious fact that $\sqrt{2} \leq \sqrt{3} \leq \sqrt{5}$.

The items 2 and 3 are agreed with our understanding of what false and implication are. The last condition is the truth condition for quantified formulas. This condition describes our intuition about the circumstance when a universal statement is true. Let us consider an example. Let us assume that we want to read the formula $A(x)$ as $x$ has a father. Our domain $M$ is the set of all people who have ever lived on the planet. Then the statement $\forall x \: A(x)$ is true since every human has a father, as the reader already knows even regardless of this post.

Now let us return to modal logic and Kripke models. Let us take a look at truth defitions for $\Box$ and $\Diamond$ one more time:
1. $\mathcal{M}, w \vdash \Diamond \varphi \Leftrightarrow \exists u \:\: w R u \: \& \: \mathcal{M}, u \models \varphi$, i.e., a formula $\Diamond \varphi$ is true at the point $w$, if there exists $R$-successor of $w$, namely, $u$ such that a formula $\varphi$ is true.
2. $\mathcal{M}, w \vdash \Box \varphi \Leftrightarrow \forall u \:\: w R u \Rightarrow \mathcal{M}, u \models \varphi$, i.e., a formula $\Box \varphi$ is true at the point $w$, if at every $R$-successor of $w$ a formula $\varphi$ is true.

In those explanations, we used first-order quantifiers over points in Kripke models informally. But we may build the bridge between Kripke models and first-order one more precisely. Let us assume that we have the following list of predicate and relation symbols:

1. A binary relation symbols $R$
2. A countable infinite set of unary predicate letters $P_i$, where $i = 0, 1, 2 \dots$

The translation from modal formulas to first-order ones is defined inductively

1. $(p_i)^{*}(w) = P_i (a)$, where $x$ is a variable
2. $(\phi \to \psi)^{*}(w) = \phi^{*}(w) \to \psi^{*}(w)$
3. $(\bot)^{*}(w) = \bot$
4. $(\Box \phi)^{*}(w) = \forall v \: (R(w, v) \to \phi^{*}(v))$

The translation for diamonds is the following one:

$(\Diamond \phi)^{*}(w) = \exists v \: (R(w, v) \land \phi^{*}(v))$

In other words, every modal formula has its first-order counterpart, a formula with one free variable $w$.

The reader can see that we just mapped modalised formulas the first-order one with respect to their truth definition. But there's the question: is the truth of formula really preserved with such a translation? Here is the lemma:

__Lemma__
Let $\mathcal{M} = \langle W, R, \vartheta \rangle$ be a Kripke model and $a \in W$, then $\mathcal{M}, a \models \phi$ if and only if $\mathcal{M}' \models_{\operatorname{FO}} \phi^{*}(a)$.

Here $\mathcal{M}'$ is the first-order model such that its domain is $W$, and interpretation for the predicate letter is agreed with the relation on an observed Kripke frame. An interpretation of unary predicate letters is agreed with the evaluation in a Kripke model as follows. $\mathcal{M}' \models P_i(w)$ if and only if $w \in \vartheta(p_i)$. Here $p_i$ is a propositional variable with the same index $i$. In other words, those unary predicate letters allow us to encode an evaluation via the first-order language.

The lemma claims that a modal formula is true at some point $w$ if only if its translation (in which a point $w$ is a parameter) in the first-order language is true in the corresponding model and, hence, satisfiable. That is, one has a bridge between truth in a Kripke modal and first-order satisfiability. If a formula is true in some model at some point, it doesn't imply its provability in the minimal normal modal logic. We would like to connect provability in ${\bf K}$ and in first-order predicate calculus. Here is the theorem proved by Johan van Benthem in the 1970s:

__Theorem__
A formula $\phi$ is provable in ${\bf K}$ iff and only the universal closure of its standard translation $\forall w \: \phi^{*}(w)$ is provable in the first-order predicate calculus.

That is, there's an embedding of minimal normal modal logic to first-order logic. The lemma and the theorem above gives the precise meaning of the analogy between modalities and quantifiers which looks informal prima facie.

#### Modal formulas and the corresponding properties of Kripke frames.

Modal formulas are closely connected with the special properties of relations on Kripke frames. We mean a modal formula is able to express the condition on a relation  in a Kripke frame. Let us consider a simple example.

Suppose we have a set $W$ with transitive relation $R$. It means that for all $a, b, c \in W$, if $a R b$ and $b R c$, then $a R c$. Let us show that formula $\Box p \to \Box \Box p$ is valid on frame $\langle W, R \rangle$ if and only if this frame is transitive. For simplicity, we will use the equivalent form written in diamonds: $\Diamond \Diamond p \to \Diamond p$. It is very easy to show that these formulas are equivalent to each other:

TODO: TrasitivityInference.jpeg

__Lemma__ Let $\mathcal{F} = \langle W, R \rangle$, then $\mathcal{F} \models \Diamond \Diamond p \to \Diamond p$ iff $R$ is transitive.

Let $\langle W, R \rangle$ be a transitive frame, $V$ be a valutation and $w \in W$. So, we have a Kripke model $\mathcal{M} = \langle W, R, V \rangle$. Suppose $\mathcal{M}, w \models \Diamond \Diamond p$. We need to show that $\mathcal{M}, w \models \Diamond p$. If $\mathcal{M}, w \models \Diamond \Diamond p$, then there exists $v \in W$ such that $w R v$ and $\mathcal{M}, v \models \Diamond p$. But, there exists $u \in W$ such that $v R u$ and $\mathcal{M}, u \models p$. Well, we have $w R v$ and $v R u$, so we also have $w R u$ by transitivity. Hence, $\mathcal{M}, w \models \Diamond p$. Consequently, $\mathcal{M}, w \models \Diamond \Diamond p \to \Diamond p$.

The converse implication is harder a little bit. Let $\mathbb{F} = \langle W, R \rangle$ be a Kripke frame such that $\mathcal{F} \models \Diamond \Diamond p \to \Diamond p$, that is, this formula is true for each valuation map. Let $w, v, u \in W$ such that $w R v$ and $v R u$. Let us show that $w R u$.

Suppose, we have the valuation such that $V(p) = \{ w \}$. So, $\langle \mathcal{F}, V \rangle, w \models \Diamond \Diamond p$. On the other hand, $\langle \mathcal{F}, V \rangle, w \models \Diamond \Diamond p \to \Diamond p$, so $\langle \mathcal{F}, V \rangle, w \models \Diamond p$. But we have only one point, where $p$ is true, $u$. So, $w R u$.

Also we may make this table that describes the correspondence between modal formulas:

| Name |Modal formula| Relation |
|--|--|--|
| ${\bf AT}$  | $\Box p \to p$                        | for all $x \in W$, $x R x$ |
| ${\bf A4}$  | $\Box p \to \Box \Box p$              | for all $x, y, z \in W$, $x R y$ and $y R z$ implies $x R z$ |
| ${\bf AD}$  | $\Box p \to \Diamond p$               | Seriality: for all $x \in W$ there exists $y \in W$ such that $x R y$ |
| ${\bf ACR}$ | $\Diamond \Box p \to \Box \Diamond p$ | Church-Rosser property (confluence): if $x R y$ and $x R z$, then there exists $x_1$ such that $y R x_1$ and $z R x_1$ |
| ${\bf AB}$  | $p \to \Box \Diamond p$               | Symmetry: for all $x, y \in W$, $x R y$ implies $y R x$ |

Let us take a look at the corresponding frame properties one a picture:

TODO: Reflexivity.jpeg
TODO: Transitivity.jpeg
TODO: Seriality.jpeg
TODO: Confluence.jpeg
TODO: Symmetry.jpeg

More formally, a modal formula $\varphi$ defines (or characterises) the class of frames $\mathbb{F}$, if for each frame $\mathcal{F}$,$\mathcal{F} \models \varphi \Leftrightarrow F \in \mathcal{F}$. That is, a formula $\Box p \to p$ characterises the class of all reflexive frames, etc.

It is clear that our list is incomplete and utter as this blog post itself, but we have described the most popular formulas and the corresponding relation properties. By the way, one may obtain new modal logics adding one of these formulae as axioms and get a botanic garden of modal logics. There are three columns in the following table. The first column is the name of the concrete logic, a sort of identification mark. The second column describes how the given logic is axiomatised. Generally, ${\bf K} \oplus \Gamma$ denotes that we extend minimal normal modal logic with formulas from $\Gamma$ as the additional axioms. The third column is the completeness theorem that claims that a given logic is the set of formulas that are valid in some class of Kripke frames.

| Name         | Logic                                                    | Frames |
|--|--|--|
| ${\bf T}$    | ${\bf K} \oplus {\bf AT}$                                | The logic of all reflexive frames                              |
| ${\bf K}4$   | ${\bf K} \oplus {\bf A}4$                                | The logic of all transitive frames                              |
| ${\bf D}$    | ${\bf K} \oplus {\bf AD} = {\bf K} \oplus \Diamond \top$ | The logic of all serial  frames                                  |
| ${\bf S}4$   | ${\bf T} \oplus {\bf A}4$ = ${\bf K}4 \oplus {\bf AT}$   | The logic of all preorders (reflexive and transitive relations) |
| ${\bf S}4.2$ | ${\bf S}4 \oplus {\bf ACR}$                              | The logic of all confluent preorders (preorders that satisfy the Church-Rosser property)                           |
| ${\bf S}5$   | ${\bf S}4 \oplus {\bf AB}$                               | The logic of all equivalence relations                          |

The fact that a given logic is the logic of some class of frames tells us that this logic is complete with respect to this class. For instance, when we tell that ${\bf T}$ is the logic of all reflexive frames, it means that any formula which is valid in an arbitrary reflexive frame is provable in ${\bf T}$. One may prove the corresponding completeness theorem ensuring that the formulas from the table are canonical ones.

A modal formula $\varphi$ is called _canonical_, if the logic $\mathcal{L} = {\bf K} \oplus \varphi$ is the logic on its canonical frame. It's not difficult to ensure that if logic has an axiomatisation with canonical formulas, then this logic is Kripke complete. For example, if we want to prove that ${\bf S}4$ is the logic of all preorders, it's enough to check that the relation in the ${\bf S}4$-canonical frame is a preorder.

Let us remember first-order logic once more. The first is to rewrite the first table above changing the properties for the third column with the relevant first-order formulas as follows:

| Name |Modal formula| Property of a frame written in the first-order language |
|--|--|--|
| ${\bf AT}$  | $\Box p \to p$                        | $\forall x \: (x R x)$ |
| ${\bf A4}$  | $\Box p \to \Box \Box p$              | $\forall x \: \forall y \: \forall z \: (x R y \land y R z \to x R z)$ |
| ${\bf AD}$  | $\Box p \to \Diamond p$               | $\forall x \: \exists y \:\: (x R y)$ |
| ${\bf ACR}$ | $\Diamond \Box p \to \Box \Diamond p$ | $\forall x \: \forall y \: \forall z \:\: (x R y \land x R z \to \exists z_1 \:\: (y R z_1 \land z R z_1))$ |
| ${\bf AB}$  | $p \to \Box \Diamond p$               | $\forall x \: \forall y \:\: (x R y \to y R x)$|

The current question we would like to ask is there some bridge between modal formulas and first-order properties of relation in a Kripke frame. Before that, we introduce a fistful of definitions.

A modal formula $\varphi$ is called _elementary_ if the class of frames which this formula characterises is defined by some first-order formula. For example, the formulas from the table above are elementary since the corresponding frame properties might be arranged as well-formed first-order formulas.

Let us define the special kind of formulas that we call Sahlqvist formulas:

A _box-atom_ is a formula of the form $\nabla p$ or $\nabla \neg p$, where $\nabla$ is a finite (possibly, empty) sequence of boxes.
A _Sahlqvist formula_ is a modal formula of the form $\Box \dots \Box (\varphi \to \psi)$. $\Box \dots \Box$ is a $n$-sequence of boxes, $n \geq 0$. $\varphi$ is formula that contains $\land$, $\lor$, $\Box$, $\Diamond$, perhaps, $0$ times. $\phi$ is constructed from box-atoms, $\land$ and $\Diamond$.

For instance, any formula from our table is a Sahlqvist formula. The example of a non-Sahlqvist formula is the McKinsey formula $\Box \Diamond p \to \Diamond \Box p$, which is also non-elementary. This formula should be a separate topic for an extensive discussion. The reader may continue such a discussion herself with the classical paper by Robert Goldblatt called [The McKinsey Axiom is not Canonical](https://www.jstor.org/stable/2274699?seq=1#page_scan_tab_contents).

__Theorem__ (Sahlqvist's theorem)
Let $\varphi$ be a Sahlqvist formula, then $\varphi$ is canonical and elementary.

Sahlqvist's theorem is extremely non-trivial to be proved accurately in detail. However, this theorem gives us crucially significant consequences. If some normal modal logic is defined with Sahlqvist formulas as axioms, then it's automatically Kripke complete since every axiom is canonical and elementary.

Moreover, any Sahlqvist formula defines some property of a Kripke frame which is definable in the first-order language. The modal definability of first-order properties in itself has certain advantages. Let us observed them concisely and slightly philosophically. As we said at the beginning, a modal language extends the propositional one with unary operators $\Box$ and $\Diamond$. As the reader could have seen, necessity and possibility have connections with universality and existence. We established this connection defining the standard translation and embedding the minimal normal modal logic into the first-order predicate logic.
On the one hand, it is a well-known result that first-order logic is undecidable. Thus, we don't have a general procedure that defines whether a given first-order formula is true or not (alternatively, provable or not). On the other hand, we have already observed the analogy between quantifiers and modalities. On the third hand (yes, I have three hands, it's practically useful sometimes), modal logics are mostly decidable as we will see a little bit later. That is, one has a method to check the validity of some binary relation properties by encoding them in modal logic. Such a way doesn't work for an arbitrary property, indeed. But a large class of such characteristics might be covered via modal language. In the second part, we also take a look at the example of a formula whose class of Kripke frames is not first-order definable.

In order to remain intellectually honest, we should note quite frankly and openly that there also exist first-order properties of Kripke frames which are undefinable in a modal language. A relation is called _irreflexive_, if it is false that $a R a$ for each $a$. The example of an irreflexive relation is the set of natural numbers with $<$, just because there is no natural number that could be less than itself, indeed.

Let us define the notion of $p$-morphism, a natural homomorphism between Kripke frames. By natural homomorphism, we mean a map that preserves a structure of Kripke frame and validness at the same time. Let us explain the idea with the precise definition and related lemma.

__Definition__ Let $\mathcal{F}_1 = \langle W_1, R_1 \rangle$, $\mathcal{F}_2 = \langle W_2, R_2 \rangle$ be Kripke frames, then $p$-morphism is a map $f : \mathcal{F}_1 \to \mathcal{F}_2$ such that:

1. $f$ is monotone, i. e., $a R_1 b$ implies $f(a) R_2 f(b)$:

TODO: Monotone.jpeg

2. $f$ has a lifting property, i. e., $f(a) R_2 c$ implies that there exists $b \in W_1$ $a R b$ and $f(b) = c$

TODO: Lift.jpeg

A $p$-mophism between Kripke models $\mathcal{M}_1 = \langle \mathcal{F}_1, v_1 \rangle$, $\mathcal{M}_2 = \langle \mathcal{F}_2, v_2 \rangle$ is a $p$-morphism
$f : \mathcal{F}_1 \to \mathcal{F}_2$ such that:

$\mathcal{M}_1, w \models p \Leftrightarrow \mathcal{M}_2, f(w) \models p$ for every variable $p$.

TODO: PMorphism.jpeg

If $f : \mathcal{F}_1 \to \mathcal{F}_2$ is a surjective $p$-morphism, then we will write $f : \mathcal{F}_1 \twoheadrightarrow \mathcal{F}_2$. By surjection, we mean a map $f : \mathcal{F}_1 \to \mathcal{F}_2$ such that for each $y \in \mathcal{F}_2$ there exists $x \in \mathcal{F}_1$ such that $f(x) = y$.

The following lemma describes a $p$-morphism behaviour with formulas that are true in models and valid in frames.

__Lemma__
1. If $f : \mathcal{M}_1 \to \mathcal{M}_2$, then $\mathcal{M}_1, w \models \varphi$ iff $\mathcal{M}_2, f(w) \models \varphi$
2. If $f : \mathcal{F}_1 \twoheadrightarrow \mathcal{F}_2$, then $\operatorname{Log}(\mathcal{F}_1) \subseteq \operatorname{Log}(\mathcal{F}_2)$

__Proof__
We prove only the first part of the lemma. Let us suppose that $\Diamond$ is a primitive modality for a technical simplicity. The base case with variables is already proved since the condition of the lemma for variables is the part of model $p$-morphism definition.

Let us assume that $\varphi \eqcirc \Diamond \psi$. We prove that $\mathcal{M}_1, w \models \Diamond \psi$ iff $\mathcal{M}_2, f(w) \models \Diamond \psi$. In other words, one needs to prove two implications:

1. If $\mathcal{M}_1, w \models \Diamond \psi$, then $\mathcal{M}_2, f(w) \models \Diamond \psi$.

Let $\mathcal{M}_1, w \models \Diamond \psi$, then there exists $v \in R_1(w)$ such that $\mathcal{M}_1, v \models \psi$. By induction hypothesis, $\mathcal{M}_2, f(v) \models \psi$. $f$ is a $p$-morphism, hence $f$ is monotone and $w R_1 v$ implies $f(w) R_2 f(v)$. Thus, $\mathcal{M}_2, f(w) \models \Diamond \psi$. We visualise the reasoning above as follows:

TODO: PMorphismLemma1.jpeg

2. If $\mathcal{M}_2, f(w) \models \Diamond \psi$, then $\mathcal{M}_1, w \models \Diamond \psi$

Let $\mathcal{M}_2, f(w) \models \Diamond \psi$. Then there exists $x \in R_2 (f(w))$ such that $f(w) R_2 x$. $f$ is a $p$-morphism and $f(w) R_2 x$, then there exists $v \in W_1$ such that $w R_1 v$ and $f(v) = x$. By induction hypothesis, $\mathcal{M}_1, v \models \psi$. But $w R_1 v$, so $\mathcal{M}_1, w \models \Diamond \psi$. Take a look at the picture.

TODO: PMorphismLemma2.jpeg

Now we show that there doesn't exist a modal formula that expresses irreflexivity of a relation.

__Lemma__
The class of all irreflexive frame is not modal definable.

__Proof__
Let $\mathcal{F}_1 = \langle \mathbb{N}, < \rangle$ be a Kripke frame, a natural numbers with less-than relation, which is obviously irreflexive. Let $\mathcal{F}_2 = \langle \{ * \}, R = \{ (*, *) \} \rangle$. Let us put $f : \mathbb{N} \to \{ * \}$ as $f(x) = *$ for all $x \in \mathbb{N}$.

TODO: NatPoint.jpeg

It is easy to see that this map is monotone and surjective. Let us check the lifting property. Let $f(x) R 0$. Let $y := x + 1$, then $x < y$ and $f(y) = 0$. Then $f$ is a $p$-morphism, but $\mathcal{F}_1$ is an irreflexive frame and $\mathcal{F}_2$ is merely reflexive point.

If the reader is interested in modal definability in more detail, then she might take into consideration the Goldblatt-Thomason theorem that connects modal definability, first-order definability with $p$-morphisms and some other operations on Kripke frames for further study.

#### Decidability in modal logic

The decidability of a formal system allows one to establish whether a formula is provable or not algorithmically. In modal logic, the famous Harrop's theorem provides the most widespread method of decidability proving. Let us introduce some definitions to formulate this theorem.

__Definition__
A normal modal logic $\mathcal{L}$ is finitely axiomatisable if $\mathcal{L} = {\bf K} \oplus \Gamma$ for some $\Gamma$, where $\Gamma$ is a finite set of formulae.

In other words, $\mathcal{L}$ is finitely axiomatisable if this logic extends minimal normal modal logic with some finite set of axioms.

__Definition__
A normal modal logic $\mathcal{L}$ has a _finite model property_ (or _finitely approximable_), if $\mathcal{L} = \operatorname{Log}(\mathbb{F})$, where $\mathbb{F}$ is some class of finite frames.

That is, a finite model property is a Kripke completeness with respect to the class of finite Kripke frames. Now we formulate the famous Harrop's theorem:

__Theorem (Harrop)__

Let $\mathcal{L}$ be a normal modal logic such that $\mathcal{L}$ is finitely axiomatisable and has a finite model property. Then $\mathcal{L}$ is decidable.

__Proof__
We provide only a quite brief proof sketch. It is clear that the set of formulas provable in $\mathcal{L}$ is enumerable since this logic merely extends ${\bf K}$ with the finite set of axioms. On the other hand, let $\varphi \notin \mathcal{L}$. Then the set $\operatorname{Bad} = \{ \varphi \in Fm \: | \: \varphi \notin \mathcal{L} \}$ is also enumerable, so far as one may enumerate finite frames such that $\mathcal{F} \models \varphi$ and $\mathcal{F} \not\vdash \varphi$, where $\varphi \in \operatorname{Bad}$.

So, if we have some finitely axiomatisable logic, then one needs to can show that this logic is complete with respect to some class of finite frames. Here, we will assume that $\Diamond$ is a primitive modality for simplicity.

__Definition__
Let $\mathcal{M} = \langle W, R, \vartheta \rangle$ be a Kripke model and $\Gamma$ a set of formulas closed under subformulas (that is, if $\varphi \in \Gamma$ and $\psi$ is a subformula of $\varphi$, then $\psi \in \Gamma$). We put the following equivalence relation:

$x \sim_{\Gamma} y \Leftrightarrow \mathcal{M}, x \models \varphi \Rightarrow \mathcal{M}, y \models \varphi$, where $\varphi \in \Gamma$

Then, a filtration of a model $\mathcal{M}$ through a set $\Gamma$ is a model $\overline{M} = \langle \overline{W}, \overline{R}, \overline{\vartheta} \rangle$, where
1. $\overline{W} = W / \sim_{\Gamma}$. In other words, $\overline{W}$ is a [quotient](https://en.wikipedia.org/wiki/Equivalence_class) by relation $\sim_{\Gamma}$, the set of equivalence classes.
2. Let $\bar{x}, \bar{y} \in \overline{W}$, then $\bar{x} \overline{R} \bar{y} \Leftrightarrow \exists w \in \bar{x} \: \exists v \in \bar{y} \:\: w R y$
3. $\overline{\vartheta}(p) = \{ \bar{x} \in \bar{W} \: | \: \exists w \in \bar{x} \: \mathcal{M}, w \models p \}$

Here is a quite obvious observation. Suppose, one have a model $\mathcal{M} = \langle W, R, \vartheta \rangle$ and its filtration $\overline{M} = \langle \overline{W}, \overline{R}, \overline{\vartheta} \rangle$ through the set of subformulas $\operatorname{Sub}(\varphi)$, where $\varphi$ is a arbitrary formula. Then $|\overline{W}| \leq 2^{|\operatorname{Sub}(\varphi)|}$.

Harrop's theorem provides the uniform method to prove that some normal modal logic is decidable if this logic is finitely axiomatisable and has finite model property. We need filtration to prove that a given logic is finitely approximable.

Here comes the first lemma about minimal filtration.

__Lemma__
Let $\mathcal{M}$ be a Kripke model and $\Gamma$ a set of formulas closed under suboformulas, then $\mathcal{M}, x \models \varphi \Leftrightarrow \overline{\mathcal{M}}, \bar{x} \models \varphi$, where $\varphi \in \Gamma$.

__Proof__
Quite simple induction on $\varphi$. Let us check this statement for $\varphi \eqcirc \Diamond \psi$.

At first, let us check the only if part. Let $\mathcal{M}, x \models \Diamond \psi$. Then there exists $y \in R(x)$ such that $\mathcal{M}, y \models \psi$. By induction hypothesis, $\overline{\mathcal{M}}, \bar{y} \models \psi$. But $\bar{x} \overline{R} \bar{y}$ by the definition of $\overline{R}$. This, $\overline{\mathcal{M}}, \bar{x} \models \Diamond \psi$.

The converse implication has the same level of completexity. Let $\overline{\mathcal{M}}, \bar{x} \models \Diamond \psi$. Then there exists $c \in \overline{R}(\bar{x})$ such that $\overline{\mathcal{M}}, c \models \psi$. Consequently, there exist $w \in \bar{x}$ and $v \in c$ such that $w R v$. By assumption, $\mathcal{M}, v \models \psi$. Thus, $\mathcal{M}, w \models \Diamond \psi$.

Now we may formulate the theorem which claims that the minimal normal modal logic is the logic of all finite Kripke frames.

__Theorem__
1. ${\bf K} = \operatorname{Log}(\mathbb{F})$, where $\mathbb{F}$ is the class of all finite Kripke frames.
2. ${\bf T} = \operatorname{Log}(\mathbb{F})$, where $\mathbb{F}$ is the class of all finite reflexive frames.
3. ${\bf D} = \operatorname{Log}(\mathbb{F})$, where $\mathbb{F}$ is the class of all finite serial frames.
4. ${\bf B} = {\bf K} \oplus p \to \Box \Diamond p = \operatorname{Log}(\mathbb{F})$, where $\mathbb{F}$ is the class of all finite symmetric frames.

__Proof__
Let ${\bf K} \not\vdash \varphi$. It means that there exists a model $\mathcal{M}$ and $x \in \mathcal{M}$ such that
$\mathcal{M}, x \not\models \psi$ according to the completeness theorem. Let $\overline{\mathcal{M}}$ be a minimal filtration of a model $\mathcal{M}$ through $\operatorname{Sub}(\psi)$, the set of subformulas of $\psi$. By the previous lemma, $\overline{\mathcal{M}}, \bar{x} \not\models \psi$. It is clear that this model is finite, since $|\overline{W}| \leq 2^{|\operatorname{Sub}(\varphi)|}$, as we observed above.

(2), (3), (4) are proved similarly, but one needs to check that a minimal filtration preserves reflexivity, seriality, and symmetry.

However, we are in trouble. We haven't already discussed a finite model property of logics that contain transitivity as an axiom. Unfortunately, a minimal filtration doesn't have to preserve the transitivity. Let $\mathcal{M} = \langle W, R, \vartheta \rangle$ be a transitive model and $\overline{M} = \langle \overline{W}, \overline{R}, \overline{\vartheta} \rangle$ a minimal filtration of $\mathcal{M}$ through $\Gamma$. Let $\bar{w}, \bar{v}, \bar{u} \in \overline{W}$ such that $\bar{w} \overline{R} \bar{v}$ and $\bar{v} \overline{R} \bar{u}$. If $\bar{w} \overline{R} \bar{v}$, then there exists $x \in \bar{w}$ and $y \in \bar{v}$ such that $x R y$. From the other hand, if $\bar{v} \overline{R} \bar{u}$, then there exists $y' \in \bar{v}$ and $z \in \bar{u}$ such that $y' R z$.

TODO: Filter1.jpeg

It is clear that $y$ doesn't have to see $y'$ within the equivalence class $\bar{u}$. Thus, a relation in minimally filtrated model isn't necessarily transitive, even if the original relation is transitive.

A solution to that problem is the transitive closure of a relation in a minimal filtration. Let us discuss what a transitive closure is. Suppose we have some set $W$ with a binary relation $R$. Generally, this relation is not transitive, but we'd like to extend it to the transitive one. What should we make? Suppose we have $x, y, z \in W$ such that $x R y$ and $y R z$. We add $x R z$ to the extended relation which we denote as $R^{+}$. We perform this action for each situation. That gives us a transitive version of a relation.

Here we going to extend a relation in a minimal filtration. This solution was proposed initially by Dov Gabbay. We will denote this closure $(\overline{R})^{+}$.

TODO: Filter2.jpeg

A transitive closure of a relation in a minimal filtration allows us to prove that ${\bf K}4$ is the logic of finite transitive frames. Firstly, we formulate the lemma that explains why a transitive closure is a good idea:

__Lemma__
Let $\mathcal{M} = \langle W, R, \vartheta \rangle$ be a transitive model and $\overline{M} = \langle \overline{W}, \overline{R}, \overline{\vartheta} \rangle$ a minimal filtration through $\Gamma$, then the following conditions hold:

1. If $a \overline{R} b$, then $a (\overline{R})^{+} b$, where $a, b \in \overline{W}$. This statement claims that a relation in a minimal filtration is a subrelation if its transitive closure.
2. If $w R v$, then $\bar{w} \overline{R} \bar{v}$. If two relation are connected with the underlying relation, then a transitive closure preserve this connection.
3. Let $\bar{w} (\overline{R})^{+} \bar{v}$ and $\mathcal{M}, w \models \Box \varphi$, then $\mathcal{M}, v \models \varphi$. Here we claim that a transitive closure should respect the truth condition for $\Box$.

Using the lemma above, it is not so hard to obtain the following theorem:

__Theorem__
1. ${\bf K}4 = \operatorname{Log}(\mathbb{F})$, where $\mathbb{F}$ is the class of all finite transitive frames
2. ${\bf S}4 = \operatorname{Log}(\mathbb{F})$, where $\mathbb{F}$ is the class of all finite preorders.
3. ${\bf S}5 = \operatorname{Log}(\mathbb{F})$, where $\mathbb{F}$ is the class of all finite equivalence relations.

The theorem above allows us to claim that the logics ${\bf K}4$, ${\bf S}4$, and ${\bf S}5$ have a finite model property. All these logics are finitely axiomatisable. Then, by Harrop's theorem, ${\bf K}4$, ${\bf S}4$, and ${\bf S}5$ are decidable. That is, one has a uniform method to provide a countermodel in which every unprovable formula fails.

#### Summary

We discussed a brief history of modal logic and its mathematical and philosophical roots. After that, we introduced the grammar of modal logic to define what
modal formula is. As we have already told, the definition of a modal language is not enough to deal with modal logic. From a syntactical perspective, we have formal proofs as derivation with axioms and inference rules. We defined normal modal logic as a set of formulas with specific limitations. As an underlying logic, we fixed minimal normal logic ${\bf K}$. Here, the other modal logics merely extend the minimal normal modal one with the additional axioms. Note that syntax doesn't answer the question about the truth of a formula. The distinction between proof and truth is a foundation of the logical culture in itself. Truth is a semantical concept.
To define the truth condition for modal formulas, we defined Kripke frames and Kripke models. After that, we formulated the completeness theorems for the list of modal logics. As usual, completeness tells us that, very roughly speaking, any valid formula is provable. The underlying modal logic ${\bf K}$ is a logic of all possible Kripke frames. Other modal logics are complete with respect to a narrower class of frames with the relation in which is restricted somehow.
Alright, we have the completeness theorem for some logic, but we also would like to define whether a given formula is provable or not algorithmically. For this purpose, we took a look at methods of proving the fact that a given normal modal logic has finite model property. Finite model property with finite axiomatisation gives us decidability according to the Harrop's theorem.

We studied the background of modal logic so that we could continue our journey in more concrete case studies. In the second part, we will overview concrete branches of modal logic, such as topological semantics, provability logic, and temporal logic. We will discuss how exactly modal logic is connected with geometry, the foundations of mathematics, and computer science.

I sincerely hope the reader managed to survive in this landscape of such an abstract and theoretically saturated material.

Follow Serokell on [Twitter](https://twitter.com/serokell) and [Facebook](https://www.facebook.com/serokell.io/) to keep in touch!
