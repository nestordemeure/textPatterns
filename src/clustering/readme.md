# CLustering

The distance betwwen two patterns is the score of the best pattern that matches both (their "father").

## Hierarchical clustering

Algorithm in o(n²).  

- Computes the distances between each pair of nodes (o(n²)) and store them in a priority queue (also store the list of existing nodes in a set).
- until there is only one node left do :
  - pop the best node from the queue o(log(n))
  - if the two node that produced this father are no more, start again (checking in the set : o(log(n)))
  - if the two nodes that produced this father exists, build a new node with this pattern and all the nodes that it matches (o(n))
  - removed the nodes collected from the set of existing nodes and add the new node (o(log(n)))
  - start again

## Incremental hierarchical clustering

### Adding a single pattern to a tree

**TODO**

### Merging two trees

**TODO**

## Coarse grained clustering

**TODO**