module Incremental

open Pattern

//-----------------------------------------------------------------------------
// TREE REPRESENTATION

/// represents a tree of patterns
type ElaboratePatternTree = {pattern : Pattern; childrens : ElaboratePatternTree list; leafs : Pattern list}

/// takes an elaborate pattern tree and outputs a simple pattern tree
let rec toPatternTree tree =
   let leafs = List.map (makePatternTree []) tree.leafs
   let childrens = List.map toPatternTree tree.childrens
   {pattern = tree.pattern; childrens = leafs @ childrens}

//-----------------------------------------------------------------------------
// INCREMENT

/// we find the most specific father we can get with a leaf
/// and output a (tree, leafs) option with the tree build on top of the new pattern and the leafs minus the leaf that matches that pattern
let addPatternToleafs leafs pattern =
   /// searches for the most specific pattern available
   let rec search bestScore bestFather bestLeaf discardedLeafs leafs =
      match leafs with
      | [] -> 
         // TODO we might want to check wether old leafs match this pattern but we should be able to presupose independence
         //{pattern = bestFather; childrens = []; leafs = [bestLeaf; pattern]} , discardedLeafs
         let matching, nonMatching = List.partition (fun p -> matchPattern p bestFather) discardedLeafs
         {pattern = bestFather; childrens = []; leafs = bestLeaf :: pattern :: matching} , nonMatching
      | leaf :: leafs ->
         let father = Pattern.commonPattern pattern leaf
         let scoreLeaf = Pattern.score father
         if scoreLeaf > bestScore then
            search scoreLeaf father leaf (bestLeaf::discardedLeafs) leafs
         else search bestScore bestFather bestLeaf (leaf::discardedLeafs) leafs
   // tries to go and search for the most specific pattern available
   match leafs with
   | [] -> None
   | leaf :: leafs ->
      let father = Pattern.commonPattern pattern leaf
      let scoreLeaf = Pattern.score father
      let result = search scoreLeaf father leaf [] leafs
      Some result

let mutable unused = [] : Pattern list

/// adds a single pattern to a tree
/// relies on the hypothesis that the pattern matches tree.pattern
let rec addPatternIntoMatchingTree tree pattern =
   let matching, nonMatching = List.partition (fun child -> matchPattern child.pattern pattern) tree.childrens
   match matching with
   | [] -> // no match, we add the pattern to the childrens
      match addPatternToleafs tree.leafs pattern with
      | Some (newChild,newLeafs) when newChild.pattern <> tree.pattern -> // we build a new, more specific, pattern
         {tree with childrens = newChild :: tree.childrens; leafs = newLeafs}
      | _ -> // no leafs or best father = tree.pattern : we add to the leafs
         {tree with leafs = pattern :: tree.leafs}
   | [child] when child.pattern = pattern -> // the pattern alreaddy exists
      tree
   | [child] -> // a single match, we go there
      let newChild = addPatternIntoMatchingTree child pattern
      let newChildrens = newChild :: nonMatching
      {tree with childrens = newChildrens}
   | _ -> // several matches, their pattern is too specific : we fuse them and insert into the result
      // TODO here we should fuse patterns and leafs properly
      // but it kills performances
      let childPattern = matching |> List.map (fun t -> t.pattern) |> List.reduce Pattern.commonPattern
      let childChildrens = List.collect (fun t -> t.childrens) matching
      let childLeafs = List.collect (fun t -> t.leafs) matching
      if childPattern <> tree.pattern then
         let newChild = {pattern = childPattern; childrens = childChildrens; leafs = []}
         //let newChild = List.fold addPatternIntoMatchingTree newChild (pattern::childLeafs)
         
         let newChild = addPatternIntoMatchingTree newChild pattern
         unused <- childLeafs @ unused
         
         let newChildrens = newChild :: nonMatching
         {tree with childrens = newChildrens}
      else
         let newChildrens = childChildrens @ nonMatching
         let newTree = {tree with childrens = newChildrens}
         //List.fold addPatternIntoMatchingTree newTree (pattern::childLeafs)
         
         unused <- childLeafs @ unused
         addPatternIntoMatchingTree newTree pattern

/// build a tree incrementaly by adding one patern ofter the other
let buildTreeIncrementaly logs =
   let tree = {pattern = Pattern.universalPattern; childrens = []; leafs=[]}
   //List.fold addPatternIntoMatchingTree tree logs |> toPatternTree
   
   let rec insert tree l =
      printfn "unused : %d" (List.length unused)
      unused <- []
      let tree = List.fold addPatternIntoMatchingTree tree l
      if List.isEmpty unused then toPatternTree tree else insert tree unused
   insert tree logs
