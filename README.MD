# Changes to the starter code

## Lambek.hs

**Step 1: deciding grammaticality.** `dec_rinv` and `dec_lfoc` follow the rules of the focused sequent calculus as detailed out in the project webpage. The two helpers, `splits` and `picks` list all the ways to cut and pick one type out of it. `choices` lists every way of picking one lexicon entry per word and `typings` keeps just the type and `isGrammatical` checks whether any of those proves the sentence type. These were mainly done by Shounak.

**Step 2: derivations.** `der_rinv` and `der_lfoc` do the same search, but every hypothesis now has a variable name attached to it and they return lambda terms instead of a binary answer. `der_lfoc` returns the list of arguments that will fill the hole, and the focus rule applies the chosen variable to them. Fresh bound variables (`a`, `b`, ...) come from `fresh` in `LExp.hs`. `derivations` gives all the terms for a sentence. Again these mainly follow the rules and logic as described on the project webpage. These were mainly done by Shounak.

**Step 3: meanings.** `readings` pairs every derivation with the meanings of the lexicon entries it used. This matters for some words because words like Hungry and Grey, have two entries with different meanings. `derivations` is just `readings` with the meanings dropped. `toPython` turns a term into a Python expression by replacing each word with its meaning from the lexicon and each lambda with a Python lambda. These were mainly done by Omar.

## ChatLLaMbda.hs

- prints `That is not a well-formed sentence` if there's no derivation, just the answer if there's exactly one, and `That sentence is ambiguous` followed by one `Sense [...]` line per derivation if there's more than one, following the example as it is described on the project page. Mainly done by Omar.

## animals.elx

We added one entry for the extension:

```
Who : q / (np \ s) = lambda p : [x for x in animals if p(x)].
```

Done by Omar.

## Use of AI

We used LLM tools during the project. In particular:

- We asked it how to approach the project and the best way to split the work, in hindsight it might've been better to go through the entire project webpage and understand it well before deciding the work split. 

- We also used it explained the rules of the calculus, especially some of the more complex notation and derivation rules. We wrote the code ourselves.

- Again for step 2 we used it to understand the derivation rules, byut again we wrote the code ourselves.

- After writing the code for step 3, we tested it ourselves and then another final check with an LLM tool to ensure it is correct, but pointed out some deviations from what was mentioned on the project webpage, eg.: we weren't handling the "ambigous" case correctly initially, along with also some output formatting corrections/improvements.

- Finally we used it to tell us what to include while we write this README, since we didn't keep a running log of every change we made from the starter code we started with.
