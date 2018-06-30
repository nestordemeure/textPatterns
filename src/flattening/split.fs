module Split
open Pattern

/// splits a tree if it it not specific enough
let rec split splitThreshold tree =
   let specif = specificity tree.pattern
   if (List.isEmpty tree.childrens) || (specif > splitThreshold) then [specif, tree.pattern] else
      List.collect (split splitThreshold) tree.childrens

/// splits a tree using the parent's pattern as a way to normalize the specificity
let splitDifferential splitThreshold tree =
   let rec split knownParent tree =
      let scoreTree = scoreTextOnly tree.pattern
      let known = max 0 (scoreTree.known - knownParent)
      let length = max 0 (- scoreTree.negativ_length - knownParent)
      let specif = float known / float length
      if (List.isEmpty tree.childrens) || (specif >= splitThreshold) then [specif, tree.pattern] else
         List.collect (split scoreTree.known) tree.childrens
   split (scoreTextOnly tree.pattern).known tree

/// splits a tree using the parent's pattern as a way to normalize the specificity
let splitDifferentialImproved maxUnknown splitThreshold tree =
   let rec split knownParent tree =
      let scoreTree = score tree.pattern
      let known = max 0 (scoreTree.known - knownParent)
      let length = max 0 (- scoreTree.negativ_length - knownParent)
      let unknown = length - known
      let specif = float known / float length
      if (List.isEmpty tree.childrens) || ((specif >= splitThreshold) && (unknown <= maxUnknown)) then [specif, tree.pattern] else
         List.collect (split scoreTree.known) tree.childrens
   split (score tree.pattern).known tree
