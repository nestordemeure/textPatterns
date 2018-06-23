module Incremental

open Pattern

//-----------------------------------------------------------------------------
/// INCREMENT

/// fuses trees by merging there childrens under a common pattern
let fuseTrees trees =
   let pattern = trees |> List.map (fun t -> t.pattern) |> List.reduce Pattern.commonPattern
   let childrens = List.collect (fun t -> t.childrens) trees
   makePatternTree childrens pattern

/// adds a single pattern to a tree
/// relies on the hypothesis that the pattern matches tree.pattern
let rec addPatternIntoMatchingTree tree pattern =
   let matching, nonMatching = List.partition (fun child -> matchPattern child.pattern pattern) tree.childrens
   match matching with
   | [] -> // no match, we add the pattern to the childrens
      let newChildrens = (makePatternTree [] pattern) :: tree.childrens
      {tree with childrens = newChildrens}
   | [child] when child.pattern = pattern -> // the pattern alreaddy exists
      tree
   | [child] -> // a single match, we go there
      let newChild = addPatternIntoMatchingTree child pattern
      let newChildrens = newChild :: nonMatching
      {tree with childrens = newChildrens}
   | _ -> // several matches, their pattern is too specific : we fuse them and then insert into the result
      let newChild = addPatternIntoMatchingTree (fuseTrees matching) pattern
      let newChildrens = newChild :: nonMatching
      {tree with childrens = newChildrens}

/// adds a single pattern to a tree
let addPatternIntoTree tree pattern =
   match matchPattern tree.pattern pattern with
   | true -> // matches the tree, easy
      addPatternIntoMatchingTree tree pattern
   | false -> // does not matches the tree, we need to build a new tree on top
      let newPattern = Pattern.commonPattern tree.pattern pattern
      let newChildrens = [tree; makePatternTree [] pattern]
      makePatternTree newChildrens newPattern

/// build a tree incrementaly by adding one patern ofter the other
let buildTreeIncrementaly logs =
   match logs with
   | [] -> failwith "Incremental.buildtree : empty list."
   | pattern :: patterns -> 
      let tree = makePatternTree [] pattern
      List.fold addPatternIntoTree tree patterns

(*
   as it currently is, the system produces very large leafs
   
   if no pattern matches, we look at the leafs
   if there is no leaf with which we can build a pattern more specific than the current pattern, we add to the leaft
   otherwise we build the most specific pattern we can and add it to the patterns
*)