module Incremental

open Pattern

//-----------------------------------------------------------------------------
/// INCREMENT

/// replaces x with y in the list
let rec replaceInList x y l =
   match l with
   | [] -> []
   | t::q when t=x -> y :: q
   | t::q -> t :: replaceInList x y q

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
      let childrens = List.collect childrenOfNode matched
      nonMatched @ refine pattern childrens

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
      let newChildrens = replaceInList child newChild tree.childrens
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
let buildTreeIncrementaly logs =
   match logs with
   | [] -> failwith "Incremental.buildtree : empty list."
   | pattern :: patterns -> 
      let tree = makePatternTree [] pattern
      List.fold addPatternIntoTree tree patterns

//-----------------------------------------------------------------------------
/// MERGE