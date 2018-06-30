module Prefix
open Pattern

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
let rec homogene trees =
   match trees with 
   | [] -> true
   | tree :: logs -> 
      let firstChar = List.head tree.pattern
      List.forall (fun tree -> List.head tree.pattern = firstChar) trees

/// replaces the first tokens of all prefix with unknown
let setFirstChar trees =
   let set tree =
      match tree.pattern with 
      | [] -> tree
      | _::q -> {tree with pattern = Unknown::q}
   List.map set trees

/// takes trees and make sure that prefix sharing the same signature starts with the same token or unknown
/// fuse tree with identical prefix
let homogenize trees =
   trees
   // homogenize prefixes
   |> List.groupBy (fun tree -> signature tree.pattern)
   |> List.collect (fun (_,trees) -> if homogene trees then trees else setFirstChar trees)
   // fuse trees with identical prefix
   |> List.groupBy (fun tree -> tree.pattern)
   |> List.map (fun (prefix,trees) -> {pattern = prefix; childrens = List.collect (fun tree -> tree.childrens) trees})

/// group tree by first char and drops the first char
let split trees =
   trees
   |> List.groupBy (fun tree -> List.head tree.pattern)
   |> List.map (snd >> List.map (fun tree -> {tree with pattern = List.tail tree.pattern}))

//-----------------------------------------------------------------------------
// CLUSTER

/// takes a number of logs and a leafsize under wich it stops refining and outputs a prefixtree
let buildTree logs =
   /// uses a list of trees to represents prefix (as pattern) and logs (as childrens)
   let trees = List.map (fun log -> makePatternTree [makePatternTree [] log] log) logs
   /// build the final tree
   let rec build trees =
      match trees with 
      | [] -> 
         makePatternTree [] universalPattern
      | [tree] -> 
         let pattern = tree.childrens |> List.map (fun tree -> tree.pattern) |> List.reduce commonPattern
         {tree with pattern = pattern}
      | _ ->
         let leafs, trees = List.partition (fun tree -> List.isEmpty tree.pattern) trees
         let leaf = {pattern=[]; childrens=leafs |> List.map (fun tree -> tree.childrens) |> List.concat}
         let childrens = trees |> homogenize |> split |> List.map build
         let childrens = if List.isEmpty leafs then childrens else (build [leaf]) :: childrens
         match childrens with
         | [] -> makePatternTree [] universalPattern
         | [child] -> child
         | _ -> 
            let pattern = childrens |> List.map (fun tree -> tree.pattern) |> List.reduce commonPattern
            makePatternTree childrens pattern
   build trees
