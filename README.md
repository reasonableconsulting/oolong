# @reasonableconsulting/oolong

Better Application Development

## Inspiration

The Elm Architecture (TEA) works well for small demo applications but "global" state atoms don't scale to very large applications. Updating the entire state atom causes huge performance issues and you eventually start having clashing state, among other problems. Many people have tried to solve this using Lenses or other ways of splitting their state and applying subsets of changes.

![Automata Theory image](https://upload.wikimedia.org/wikipedia/commons/thumb/a/a2/Automata_theory.svg/450px-Automata_theory.svg.png)

TEA, Redux, etc are Finite-State Machines, but as shown in the diagram above, they are a subset of Pushdown Automatons. While reading through the Pushdown Automaton wikipedia page, I came to the realization that we already have one available to us in the browser, the [History API](https://developer.mozilla.org/en-US/docs/Web/API/History).

If we leverage this API, we can build a more complete application development platform upon it. Using the History Pushdown Automaton, the state stack is persisted and can be traversed - instead of the state being ephemeral and needing extra code for handling navigation and hard refreshes.

The goal of this library is to guide application developers into thinking about any "global" state in the context of the URL - from which they must derive their initial state and reflect back any state updates. This allows the entire application to both be rendered from a static route or from History API changes.
