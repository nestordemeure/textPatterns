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

TODO : write clear description.

- Under a size it uses Hierarchical clustering.
- Over a size it does a single pass of coarse grained clustering and then retries.
- Using incremental clustering we can divide the task and fuse the results.

## TODO

- Add classical hierarchical clustering (o(n²))
- Add coarse graining clustering when the number of elements is too big to use hierarchical approach.
- Add incremental clustering

- The current version produces all father patterns blindly and not just the one that have a chance of suceeding : 2^n it should probably be dropped.
- Clean-up the code.
- Test with actual logs.
- Parallelise.
- Implement tree cut-off with a specificity criteria.
- Add token typing to detect numbers, dates, etc
- clean-up references

## References

- ??? for the hierarchical clustering on lines of text
- Logmine for the pass of coarse grained clustering and the pass of type deduction to simplify logs
- incremental hierarchical clusterign can be found in ???
