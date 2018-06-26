module TopDown

open Pattern

//-----------------------------------------------------------------------------
// CLUSTERING

/// returns true if the two logs are within the given distance
let rec isInDistance distMax log1 log2 =
   if distMax < 0 then false else 
      match log1, log2 with
      | [], [] -> true
      | [], l | l, [] -> isInDistance (distMax - List.length l) [] []
      | t1::q1, t2::q2 when t1=t2 -> isInDistance distMax q1 q2
      | _::q1, _::q2 -> isInDistance (distMax-1) q1 q2

/// returns true if the cluster contains an element close enough to the log
let isInCluster distMax log cluster =
   List.exists (isInDistance distMax log) cluster

/// add a log to the cluster list
let addToClusters distMax clusters log =
   let matched, unmatched = List.partition (isInCluster distMax log) clusters
   let newCluster = log :: (List.concat matched)
   newCluster :: unmatched

/// gather elements in clusters
let buildClusters distMax logs =
   List.fold (addToClusters distMax) [] logs

//-----------------------------------------------------------------------------
// TREE

/// takes a list of logs and builds a tree
let treeOfCluster cluster =
   let pattern = List.reduce Pattern.commonPattern cluster
   let childrens = List.map (Pattern.makePatternTree []) cluster
   printfn "%s" (Output.stringOfPattern pattern)
   Pattern.makePatternTree childrens pattern

/// builds a tree with a given maximum distance
let buildTree distMax logs =
   let clusters = buildClusters distMax logs
   let childrens = List.map treeOfCluster clusters
   let pattern = childrens |> List.map (fun c -> c.pattern) |> List.reduce Pattern.commonPattern
   Pattern.makePatternTree childrens pattern