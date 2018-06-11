module Coarse 

open Pattern

//-----------------------------------------------------------------------------
// COARSE

/// compute a hamming distance between two patterns : 
/// number of similar characters normalized by the length of the longest pattern
let hammingDistance pattern1 pattern2 =
   let rec distanceRec l1 l2 length count =
      match l1,l2 with
      | [], l | l, [] ->
         let length = length + List.length l
         float (length - count) / float length
      | t1::q1, t2::q2 when t1=t2 ->
         distanceRec q1 q2 (length+1) (count+1)
      | t1::q1, t2::q2 ->
         distanceRec q1 q2 (length+1) count
   distanceRec pattern1 pattern2 0 0

/// takes a list of pattern trees and regroup them using a similarity measure
let clusteringPass distanceThreshold treeList =
   /// finds a cluster whose representant is closer than distanceThreshold and puts tree in it
   /// otherwise create a new cluster
   let rec addToClusters clusters tree =
      match clusters with
      | [] -> 
         [(tree, [])]
      | (tree2,childrens)::q when hammingDistance tree.pattern tree2.pattern < distanceThreshold -> 
         (tree2,tree::childrens) :: q 
      | t :: q -> 
         t :: addToClusters q tree
   /// returns the common pattern
   let treeOfCluster (tree,childrens) = 
      if List.isEmpty childrens then tree else
         let childrens = tree :: childrens
         let pattern = childrens |> List.map (fun t -> t.pattern) |> List.reduce commonPattern
         {pattern=pattern; childrens=childrens}
   /// reduces the number of element with a pass of clustering
   treeList |> List.fold addToClusters [] |> List.map treeOfCluster

// try having cluster representent = cluster mean pattern

/// builds a tree by sucessive pass of coarse clustering until it converges
/// an unsucessful pass, requiring relaxation, is a o(n²) operation
let buildTree logs =
   let distanceThreshold = 0.01
   let treeList = List.map (makePatternTree []) logs
   let rec build distanceThreshold treeList =
      match treeList with 
      | [] -> failwith "Coarse.buildTree : not enough elements"
      | [tree] -> tree
      | _ -> 
         let newTreeList = clusteringPass distanceThreshold treeList
         if List.length newTreeList < List.length treeList then 
            printfn "Sucessfull pass"
            build (1.3*distanceThreshold) newTreeList
         else
            //let newThreshold = distanceThreshold + 0.1
            let newThreshold = 1.3*distanceThreshold
            printfn "Relaxation %f->%f" distanceThreshold newThreshold 
            build newThreshold treeList
   build distanceThreshold treeList