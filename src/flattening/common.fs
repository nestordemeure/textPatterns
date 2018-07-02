module Common
open Pattern

(*
   based around the idea that if all pair of children have the same father then it is the correct father and we have a true group
   if we have only two childrens leaf we accept the father by default
   if we have only two childrens non leaf we reject by default
*)

/// returns true if a tree's childrens have all the same father
/// NOTE linear approximation, the exact version would test all pairs of childrens
let isTrueGroup tree =
   let rec isGroup childrens =
      match childrens with 
      | [] | [_] -> true 
      | c1::c2::childrens when commonPattern c1.pattern c2.pattern = tree.pattern -> isGroup (c2::childrens)
      | _ -> false 
   isGroup tree.childrens

/// returns true if a tree is a leaf
let isLeaf tree = 
   List.isEmpty tree.childrens

/// returns the list of leafs in a tree
let rec collectLeafs tree =
   if isLeaf tree then [tree.pattern] else List.collect collectLeafs tree.childrens

/// splits a tree bottom up
/// too pessismistic because of coarse groups (could be solved by adding some hierarchical clustering)
let split tree =
   let rec flatten tree =
      let flatTree = {tree with childrens = List.map flatten tree.childrens}
      if (List.length tree.childrens < 3) && (not <| List.forall isLeaf tree.childrens) then flatTree //not enough info
      elif (List.forall isLeaf flatTree.childrens) && (isTrueGroup flatTree) then makePatternTree [] flatTree.pattern //valid group
      else flatTree //invalid group flattened
   tree |> flatten |> collectLeafs
