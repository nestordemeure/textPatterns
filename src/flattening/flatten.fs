module Flatten
open Pattern

/// returns true of a pattern :
/// - is a leaf
/// - has only leaf childrens (direct node)
/// - has only leafs or direct node childrens
let canBeFlatten tree =
   let isLeaf tree = List.isEmpty tree.childrens
   let isDirectSubPattern tree = isLeaf tree || List.forall isLeaf tree.childrens
   //isDirectSubPattern tree || List.forall isDirectSubPattern tree.childrens
   isDirectSubPattern tree || ( (List.forall isDirectSubPattern tree.childrens) && (List.exists isLeaf tree.childrens) )

/// return the pattern if the tree satisfies our criteria for flattening or recurse in the childrens
let rec flatten tree =
   if canBeFlatten tree then [tree.pattern] else List.collect flatten tree.childrens