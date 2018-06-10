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

Adds one element after the other to the tree. This algorithm can deal with much large number of element (still it has not a linear complexity) but the tree might fall into local optima.

To avoid this phenomena, one can shuffle the elements, build a tree with a few hundred elements using hierarchical clustering and then use the incremental method. The hope is that using a good basis will reduce the sensibility to local minimums.

### Merging two trees

**TODO**

## Coarse grained clustering

Algorithm that is both solid, scalable and simple.

The similarity between two lines is the number of coinsciding elements divided by the length of the longest line (hamming similarity).  

Each cluster is represented by an arbitrary element and a list of members, a new element is added to a cluster if its similarity to the cluster's representent is over the current threshold.  

During a pass we go over all elements (starting with a threshold of 0.9) adding them to an existing cluster or, if they find no cluster, building them a new cluster.
We run consecutive passes until convergence, we then reduce the threshold by 0.1.
We stop when there is only one element left.

Note that once the number of elements goes below a threshold, we could go back to hierarchical clustering in order to get better quality solutions (using the coarse clustering only to simplify the original problem).