module Hierarchical

open Pattern

//-----------------------------------------------------------------------------
/// DISTANCES

/// represents the distance between two node (indicated by their index)
type Distance = {index1:int; index2:int; father:Pattern}

/// adds the distance betwwen the two nodes to the priority queue
let computeDistance (index1,node1) distances (index2,node2) =
   let father = commonPattern node1.pattern node2.pattern
   let evaluation = score father
   PriorityQueue.push evaluation {index1=index1;index2=index2;father=father} distances

/// add the distances between the node and each element of the node list to the priority queue
let computeDistances indexedNode distances indexedNodes =
   List.fold (computeDistance indexedNode) distances indexedNodes

/// fills a priority queue with the (distance,pattern) associated with all pairs of elements
/// WARNING o(n²) operation
let computeAllDistances indexedNodes =
   /// computes the distance between all pairs of element and add them to a priority queue
   let rec allDists nodes distances =
      match nodes with
      | [] -> distances
      | inode :: nodes ->
         let distances = computeDistances inode distances nodes
         allDists nodes distances
   allDists indexedNodes PriorityQueue.empty

//-----------------------------------------------------------------------------
// TREE BUILDING

/// takes a list of logs and outputs a pattern tree
/// uses a classical hierarchical clustering algorithm
/// relies on memoization via a set and a priority queue to reduce the complexity from o(n^3) to o(n^2)
let buildTree logs =
   printfn "starting to build tree"
   let indexedNodes = List.map (makePatternTree []) logs |> List.indexed // indexed list of nodes
   let distances = computeAllDistances indexedNodes // priority queue containing all distances between nodes
   let existingNodes = set [0 .. List.length indexedNodes-1] // set of all existing node numbers
   let currentNodeNumber = -1 // node index that is free to use
   printfn "distances computed"
   /// recurcively reduces the number of nodes untel there is only one left
   let rec build distances existingNodes currentNodeNumber indexedNodes =
      match indexedNodes with
      | [] -> makePatternTree [] universalPattern
      | [(index,tree)] -> tree
      | _ ->
         let (evaluation,dist), distances = PriorityQueue.unsafePop distances
         if not ((Set.contains dist.index1 existingNodes) && (Set.contains dist.index2 existingNodes)) then
            // this distance refers to nodes that do not exist anymore
            build distances existingNodes currentNodeNumber indexedNodes
         else
            let pattern = dist.father
            let matched, nonMatched = List.partition (fun (_,node) -> matchPattern pattern node.pattern) indexedNodes
            
            // debug
            if (List.length indexedNodes) = (1 + List.length nonMatched) then
               printfn "failing pattern is   \"%s\" who matched %d elements" (Output.stringOfPattern pattern) (List.length matched)
               let index1 = dist.index1
               let index2 = dist.index2
               let missedPattern = List.head (List.filter (fun (i,p) -> i = index1 || i = index2) nonMatched) |> snd
               printfn "unmatched pattern is \"%s\"" (Output.stringOfPattern missedPattern.pattern)
               let missedPattern = List.head matched |> snd
               printfn "unmatched pattern is \"%s\"" (Output.stringOfPattern missedPattern.pattern)

            let newIndexedNode = currentNodeNumber, makePatternTree (List.map snd matched) pattern
            let indexedNodes = newIndexedNode :: nonMatched
            let distances = computeDistances newIndexedNode distances nonMatched
            let existingNodes = List.fold (fun existing (index,_) -> Set.remove index existing) (Set.add currentNodeNumber existingNodes) matched
            let currentNodeNumber = currentNodeNumber - 1
            build distances existingNodes currentNodeNumber indexedNodes
   build distances existingNodes currentNodeNumber indexedNodes
