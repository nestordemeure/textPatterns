# Scalable hierarchical clustering to extract patterns from lines of text

An algorithm that find patterns between lines of text and produces a tree with the hierarchy of patterns.

## Strengths

- Fully unsupervised.
- Produces good quality results.
- Can scale to millions of lines.

## Demo

```
"I like kittens!"
"I like dogs?"
"I like cats!"
"I like ham and jam."
"I like ham but not jam."
```

Becomes :

```
->"I like %s"
 ├─>"I like %s!"
 │ ├─>"I like kittens!"
 │ └─>"I like cats!"
 ├─>"I like ham %s jam."
 │ ├─>"I like ham and jam."
 │ └─>"I like ham but not jam."
 └─>"I like dogs?"
```

## Algorithm

### Output

The patterns are displayed in the form `"Words and %s"` where `%s` is assumed to mean one or more tokens (unless it is in last position in wich case it could also mean no tokens at all).  

We call token :

- one or more letters
- one or more digits
- a single punctuation element
- one or more spaces
- one or more char that have not been captured by the previous descriptions

### Scoring

A pattern is considered better than another pattern if it contains more known tokens and, in case of equality, if it is shorter (implying a higher density of known tokens).  

The specificity of a pattern (number of known tokens divided by number of tokens) might feel like a better scoring function however, with large number of tokens, it does not focus on the preservation of the existing tokens leading to lower quality results.

### Scaling

Several strategies are used in order to be able to deal with very large number of elements :

- Under a size it uses Hierarchical clustering.
- Over a size it does a single pass of coarse grained clustering and then retries.
- Using incremental clustering we can divide the task and fuse the results.

## TODO

- Add coarse graining clustering when the number of elements is too big to use hierarchical approach.
- Add incremental clustering
- Clean-up the code.
- Parallelise.
- Implement tree cut-off with a specificity criteria.
- Add token typing to detect numbers, dates, etc
- clean-up references
- add proper algorithm description
- add exportation of pattern in regexp form

## References

- [???]() for the hierarchical clustering of lines of text.
- [LogMine: Fast Pattern Recognition for Log Analytics](https://dl.acm.org/citation.cfm?id=2983323.2983358) for the idea of coarse grained clustering and the idea of type deduction to simplify logs.
- Incremental hierarchical clustering aplied to texts can be found in [???]().
