module Prefix
open Pattern

type PrefixTree =
   | Leaf of (Token list) list
   | Node of isLEaf:bool * childrens:Map<Token, PrefixTree>

//-----------------------------------------------------------------------------
// PREFIX

/// extracts the signature of a log
let signature log =
   match log with 
   | [_] | [] -> Unknown, Unknown
   | [_;t1] -> t1, Unknown
   | _::t1::t2::_ -> t1,t2

/// checks if all the logs of a list start with the same token
/// fail if one of them is empty
let rec homogene logs =
   match logs with 
   | [] -> true
   | (token::_) :: logs -> List.forall (fun log -> List.head log = token) logs

/// replaces the first tokens of all logs with unknown
let setFirstChar logs =
   let set log =
      match log with 
      | [] -> log
      | _::q -> Unknown::q
   List.map set logs

/// takes logs and makes sure that logs sharing the same signature starts with the same token or unknown
let homogenise logs =
   logs
   |> List.groupBy signature
   |> List.collect (fun (_,logs) -> if homogene logs then logs else setFirstChar logs)

//-----------------------------------------------------------------------------
// CLUSTER

/// takes a number of logs and a leafsize under wich it stops refining and outputs a prefixtree
/// the idea is to keep the leaf size as large as possible, to build the tree using this coarse method and then to expend leafs with hierarchical clustering
let rec buildPrefixTree maxLeafSize logs =
   if List.length logs <= maxLeafSize then Leaf logs else 
      let isLeaf = List.exists List.isEmpty logs
      let logs = logs |> List.filter (List.isEmpty >> not) |> homogenise |> List.distinct
      let childrens = logs |> List.groupBy List.head |> List.map (fun (key,logs) -> key, buildPrefixTree maxLeafSize (List.map List.tail logs)) |> Map.ofList
      Node (isLeaf, childrens)

/// takes a prefix tree and outputs a pattern tree
/// for interoperability with existing code and algorithms
let rec treeOfPrefix prefixRev prefixTree =
   match prefixTree with 
   | Leaf [log] ->
      let prefix = List.rev prefixRev
      let pattern = prefix @ log
      makePatternTree [] pattern
   | Leaf logs -> 
      let prefix = List.rev prefixRev
      let logs = List.map (fun log -> prefix @ log) logs
      let pattern = List.reduce commonPattern logs
      let childrens = List.map (makePatternTree []) logs
      makePatternTree childrens pattern
   | Node (false, childrens) when Map.count childrens = 1 -> 
      childrens |> Map.toList |> List.head |> (fun (token,tree) -> treeOfPrefix (token::prefixRev) tree)
   | Node (isLeaf, childrens) -> 
      let childrens = childrens |> Seq.map (fun kv -> treeOfPrefix (kv.Key::prefixRev) kv.Value) |> Seq.toList
      let childrens = if isLeaf then (makePatternTree [] (List.rev prefixRev)) :: childrens else childrens
      let pattern = childrens |> List.map (fun c -> c.pattern) |> List.reduce commonPattern
      makePatternTree childrens pattern

//-----------------------------------------------------------------------------
// TREE

let buildTree maxLeafSize logs =
   printfn "clustering"
   let prefixTree = buildPrefixTree maxLeafSize logs
   printfn "tree building"
   treeOfPrefix [] prefixTree