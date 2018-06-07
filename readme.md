# Hierarchical text-pattern extraction

An algorithm that find patterns between lines of text and produces a tree with the hierarchy of patterns.

## Strengths

- Fully unsupervised.
- Produces good quality results.
- Requires a strict minimum of communication making efficent parallelism possible (with a parallel map and fold).

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

## TODO

- The current version produces all father patterns blindly and not just the one that have a chance of suceeding.
- Clean-up the code.
- Test with actual logs.
- Parallelise.
- Implement tree cut-off with a specificity criteria.
- Add tree typing to detect numbers, dates, etc
