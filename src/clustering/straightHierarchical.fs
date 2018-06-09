
open Pattern

//-----------------------------------------------------------------------------
// TREE BUILDING

/// takes pattern and outputs a pattern that :
// - matches at least two logs
// - maximise score
let findBestPattern treeList =
   let bestPattern = universalPattern
   let bestScore = score bestPattern
   let candidates = Set.empty
   /// if this pattern is or could become better than the best known, it is stored
   let findBest previousCandidates (bestPattern, bestScore, candidates) pattern =
      let newScore = score pattern
      if bestScore > newScore then
         (bestPattern, bestScore, candidates)
      elif Seq.contains pattern previousCandidates then
         (pattern, newScore, candidates)
      else
         let newCandidates = Set.add pattern candidates
         (bestPattern, bestScore, newCandidates)
   /// generate all potential father patterns and find the best one
   let findBestInTree (bestPattern, bestScore, candidates) tree =
      allCommonPatterns tree.pattern
      |> Seq.fold (findBest candidates) (bestPattern, bestScore, candidates)
   treeList
   |> List.fold findBestInTree (bestPattern, bestScore, candidates)
   |> fun (bestPattern,_,_) -> bestPattern

/// takes a list of logs and outputs a pattern tree
let buildTree logs =
   let treeList = List.map (makePatternTree []) logs
   let rec build treeList =
      match treeList with
      | [] -> makePatternTree [] universalPattern
      | [tree] -> tree
      | _ ->
         let bestPattern = findBestPattern treeList
         let matched, nonMatched = List.partition (fun tree -> matchPattern bestPattern tree.pattern) treeList
         let newTree = makePatternTree matched bestPattern
         build (newTree :: nonMatched)
   build treeList
