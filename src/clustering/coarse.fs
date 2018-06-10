module Coarse 

open Pattern

//-----------------------------------------------------------------------------
// COARSE

/// compute a hamming distance between two patterns : 
/// number of similar characters normalized by the length of the longest pattern
let hammingSimilarity pattern1 pattern2 =
   let rec distanceRec l1 l2 length count =
      match l1,l2 with
      | [], l | l, [] ->
         let length = length + List.length l
         float count / float length
      | t1::q1, t2::q2 when t1=t2 ->
         distanceRec q1 q2 (length+1) (count+1)
      | t1::q1, t2::q2 ->
         distanceRec q1 q2 (length+1) count
   distanceRec pattern1 pattern2 0 0

/// takes a list of pattern trees and regroup them using a similarity measure
let clusteringPass similarityThreshold treeList =
   /// finds a cluster whose representant is closer than distanceThreshold and puts tree in it
   /// otherwise create a new cluster
   let rec addToClusters clusters tree =
      match clusters with
      | [] -> 
         [(tree, [])]
      | (tree2,childrens)::q when hammingSimilarity tree.pattern tree2.pattern > similarityThreshold -> 
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

/// builds a tree by sucessive pass of coarse clustering until it converges
let buildTree logs =
   let similarityThreshold = 0.9
   let treeList = List.map (makePatternTree []) logs
   let rec build similarityThreshold treeList =
      match treeList with 
      | [] -> failwith "Coarse.buildTree : not enough elements"
      | [tree] -> tree
      | _ -> 
         let newTreeList = clusteringPass similarityThreshold treeList
         if List.length newTreeList < List.length treeList then 
            printfn "Sucessfull pass"
            build similarityThreshold newTreeList
         else
            let newSimilarity = similarityThreshold-0.1
            printfn "Relaxation %f->%f" similarityThreshold newSimilarity 
            build newSimilarity treeList
   build similarityThreshold treeList