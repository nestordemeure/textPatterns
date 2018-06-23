module Incremental

open Pattern

//-----------------------------------------------------------------------------
// TREE REPRESENTATION

/// represents a tree of patterns
type ElaboratePatternTree = {pattern : Pattern; childrens : ElaboratePatternTree list; leafs : Pattern list}

/// takes an elaborate pattern tree and outputs a simple pattern tree
let rec toPatternTree tree =
   let childrens = List.map toPatternTree tree.childrens
   let leafs = if not <| List.isEmpty childrens then [] else List.map (makePatternTree []) tree.leafs
   {pattern = tree.pattern; childrens = leafs @ childrens}

//-----------------------------------------------------------------------------
// INCREMENT

/// we find the most specific father we can get with a leaf
/// the father as to be strictly more specific than treePattern
/// and output a (tree, leafs) option with the tree build on top of the new pattern and the leafs minus the leaf that matches that pattern
let addPatternToleafs leafs treePattern pattern =
   match leafs with
   | [] -> None
   | _ ->
      //let father = leafs |> List.map (Pattern.commonPattern pattern) |> List.maxBy Pattern.score
      let leaf, leafs = Functions.extractMax (Pattern.commonPattern pattern >> Pattern.score) leafs
      let father = Pattern.commonPattern pattern leaf
      if father = treePattern || Pattern.score father < Pattern.score treePattern then None else
         let matching, nonMatching = List.partition (fun p -> matchPattern p father) leafs
         let child = {pattern = father; childrens = []; leafs = pattern :: leaf :: matching} 
         Some (child, nonMatching)

/// adds a single pattern to a tree
/// relies on the hypothesis that the pattern matches tree.pattern
let rec addPatternIntoMatchingTree tree pattern =
   let matching, nonMatching = List.partition (fun child -> matchPattern child.pattern pattern) tree.childrens
   match matching with
   | [] -> // no match, we add the pattern to the childrens
      match addPatternToleafs tree.leafs tree.pattern pattern with
      | Some (newChild,newLeafs) -> // we find a new, more specific, pattern with the leafs
         {tree with childrens = newChild :: tree.childrens; leafs = newLeafs}
      | _ -> // we add pattern to the leafs
         {tree with leafs = pattern :: tree.leafs}
   | [child] -> // a single match, we go there
      let newChild = addPatternIntoMatchingTree child pattern
      let newChildrens = newChild :: nonMatching
      {tree with childrens = newChildrens}
   | _ -> // several matches, their pattern is too specific : we fuse them and insert into the result
      let childPattern = matching |> List.map (fun t -> t.pattern) |> List.reduce Pattern.commonPattern
      let childChildrens = List.collect (fun t -> t.childrens) matching
      let childLeafs = List.collect (fun t -> t.leafs) matching
      // the fusion produces a valid pattern
      if childPattern <> tree.pattern && Pattern.score childPattern >= Pattern.score tree.pattern then
         let newChild = {pattern = childPattern; childrens = childChildrens; leafs = childLeafs}
         let childLeafs, newLeafs = List.partition (fun leaf -> matchPattern childPattern leaf) tree.leafs
         let newChild = List.fold addPatternIntoMatchingTree newChild (pattern::childLeafs)
         let newChildrens = newChild :: nonMatching
         {tree with childrens = newChildrens; leafs = newLeafs}
      // the fusion is too vague, we cut those patterns (rarely appends)
      else
         // TODO we suppose no leaf match a pattern from another branch but they could still be combined together
         let newChildrens = childChildrens @ nonMatching
         let newLeafs = childLeafs @ tree.leafs
         let newTree = {tree with childrens = newChildrens; leafs = newLeafs}
         addPatternIntoMatchingTree newTree pattern

/// build a tree incrementaly by adding one patern ofter the other
let buildTreeIncrementaly logs =
   let tree = {pattern = Pattern.universalPattern; childrens = []; leafs=[]}
   List.fold addPatternIntoMatchingTree tree logs |> toPatternTree
   (*let rec insert i imax tree l =
      match l with
      | [] -> toPatternTree tree
      | t::q -> 
         printfn "%d/%d" i imax
         let tree = addPatternIntoMatchingTree tree t
         insert (i+1) imax tree q
   insert 1 (List.length logs) tree logs*)
