module Incremental

open Pattern

//-----------------------------------------------------------------------------
/// INCREMENT

/// returns the list of childrens of a node
/// if a node has no children, we return a list containing the node
let childrenOfNode node =
   match node.childrens with
   | [] -> [node]
   | _ -> node.childrens

/// takes a list of patterns and cut level until at most one of them matches our target pattern
let rec refine pattern treeList =
   let matched, nonMatched = List.partition (fun tree -> matchPattern tree.pattern pattern) treeList
   match matched with 
   | [] | [_] -> // at most one match
      treeList
   | _ -> // too many matches, we refine them
      // refining just the matched childrens is a lot quicker but lead to local optima (this might be avoided by shuffling)
      let childrens = List.collect childrenOfNode matched
      nonMatched @ refine pattern childrens
      // refining everyone can degrade into a full tree recomputation
      //refine pattern (List.collect childrenOfNode treeList)

/// adds a single pattern to a tree
/// relies on the hypothesis that the pattern matches tree.pattern
let rec addPatternIntoMatchingTree tree pattern =
   let matchingChildren = List.filter (fun child -> matchPattern child.pattern pattern) tree.childrens
   match matchingChildren with
   | [] -> // no match, we rebuild the tree
      let newChildrens = (makePatternTree [] pattern) :: tree.childrens
      Hierarchical.TreeOfNodes newChildrens
   | [child] when child.pattern = pattern -> // an exact match, nothing to do
      tree
   | [child] -> // a single match, we go there
      let newChild = addPatternIntoMatchingTree child pattern
      let newChildrens = Functions.replaceInList child newChild tree.childrens
      if newChild.pattern = child.pattern then {tree with childrens = newChildrens}
      else Hierarchical.TreeOfNodes newChildrens
   | _ -> // several matches, we refine the childrens until there is at most one match and we reiterate
      let newChildrens = refine pattern tree.childrens
      addPatternIntoMatchingTree {tree with childrens = newChildrens} pattern

/// adds a single pattern to a tree
let addPatternIntoTree tree pattern =
   match matchPattern tree.pattern pattern with
   | true -> // matches the tree, easy
      addPatternIntoMatchingTree tree pattern
   | false -> // does not matches the tree, we need to build a new tree on top
      let newChildrens = (makePatternTree [] pattern) :: childrenOfNode tree
      Hierarchical.TreeOfNodes newChildrens

/// build a tree incrementaly by adding one patern ofter the other
/// NOTE : it is beneficial to shuffle the logs before insertion
let buildTreeIncrementaly logs =
   match logs with
   | [] -> failwith "Incremental.buildtree : empty list."
   | pattern :: patterns -> 
      let tree = makePatternTree [] pattern
      List.fold addPatternIntoTree tree patterns
      // with progress
      //let mutable i = 1
      //let imax = List.length patterns |> float
      //List.fold (fun t p -> i <- i+1; printfn "Progress : %.2f percents" ((float i * 100.) / imax); addPatternIntoTree t p) tree patterns

//-----------------------------------------------------------------------------
/// MERGE

/// splits a list into two parts of similar lengths
let split logs =
   let rec splitRec res1 res2 logs =
      match logs with
      | [] -> res1, res2
      | t::q -> splitRec res2 (t::res1) q
   splitRec [] [] logs

/// merges two trees
let mergeTree tree1 tree2 =
   tree1 // TODO

/// builds a tree by :
/// - building a tree with hierarchical clustering if the number of elements is lower than leergeTree
/// - cutting the log into two parts, building a tree for each recurcively and then merging them
/// akin to a fuse sort algorithm
let rec buildTreeMerge minSize logs =
   if List.length logs <= minSize then Hierarchical.buildTree logs else
      let logs1, logs2 = split logs
      let newLogs1 = buildTreeMerge minSize logs1
      let newLogs2 = buildTreeMerge minSize logs2
      mergeTree newLogs1 newLogs2