module Pattern

/// represents a token
type Token =
   | Word of string
   | Space
   | Month
   | LPID
   | NumberInteger
   | NumberFloat
   | NumberHexadecimal of length:int
   | Path
   | Unknown // match one or more tokens unless it comes at the end of the line in which case it can also match 0 tokens

/// represents a pattern 
type Pattern = Token list

//-----------------------------------------------------------------------------
// PATTERN

/// the least specific pattern, ever
let universalPattern = [Unknown]

/// returns true if the list of tokens matches the pattern
/// unknown is assumed to match one or more tokens unless it comes at the end
let matchPattern pattern tokens =
   let memory = System.Collections.Generic.Dictionary()
   let rec isMatch depthp pattern deptht tokens =
      if memory.ContainsKey (depthp,deptht) then memory.[(depthp,deptht)] else
         match pattern, tokens with
         | [], [] | [Unknown], [] | [], [Unknown] -> 
            // we accept to match empty tokens when they are sufixes (a similar mecanism for suffixes would be good)
            true
         | Unknown :: newPattern, _ :: newTokens ->
            let result = (isMatch (depthp+1) newPattern (deptht+1) newTokens) // match 1 token (redondant but usefull to express match with empty list)
                         || (isMatch (depthp) pattern (deptht+1) newTokens) // match 1+ tokens
            memory.[(depthp,deptht)] <- result
            result
         | p :: pattern, t :: tokens when p = t -> 
            isMatch (depthp+1) pattern (deptht+1) tokens
         | _ -> false
   isMatch 0 pattern 0 tokens

//-----------------------------------------------------------------------------
// SCORING

/// represents the evalutation of a pattern
/// Note : the type is layed out by order of priority in comparaisons
type Score = 
   {known : int; negativ_length : int}

/// returns the score of a pattern
let score pattern =
   let mutable length = 0
   let mutable known = 0
   let count t =
      length <- length + 1
      if t <> Unknown then known <- known + 1
   List.iter count pattern
   {known=known; negativ_length= -length}

/// return the fraction of known token in a pattern : the higher the better
let specificity pattern =
   let sc = score pattern
   float sc.known / float -sc.negativ_length

/// retruns true is the word contains only ascii characters
let isTextOnly w =
   let isTextChar c =
      ((c >= '0') && (c <= '9')) || ((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z'))
   String.forall isTextChar w

/// score that does not take Word whose text contains non ascii character into account
/// closer to human intuition about specificity (spaces are not seen as interesting tokens by humans)
let scoreTextOnly pattern =
   let mutable length = 0
   let mutable known = 0
   let count t =
      match t with
      | Unknown -> length <- length + 1
      | Word w when not (isTextOnly w) -> ()
      | _ -> known <- known + 1 ; length <- length + 1
   List.iter count pattern
   {known=known; negativ_length= -length}

//-----------------------------------------------------------------------------
// PATTERN BUILDING

/// add unknown in front of a pattern
/// avoid adding it twice
let addUnknown pattern =
   match pattern with
   | Unknown :: _ -> pattern
   | _ -> Unknown :: pattern

/// add a token or the unknown in front of a pattern, returns the list of results
/// fuses unknowns
let addToken token pattern =
   match token, pattern with
   | Unknown, Unknown :: _ -> [pattern]
   | Unknown, _ -> [Unknown :: pattern]
   | _, Unknown :: _ -> [token :: pattern; pattern]
   | _ -> [token :: pattern; Unknown :: pattern]

/// returns the most specific pattern that matches both patterns
/// uses memoization to reduce complexity to o(n²)
/// TODO dynamic programming could greatly increase performances
let commonPattern pattern1 pattern2 =
   let memory = System.Collections.Generic.Dictionary<int*int*bool,(int*int)*Pattern>()
   /// memorise a result
   let memorise depth1 depth2 followsUnknow result =
      memory.[(depth1,depth2,followsUnknow)] <- result
      result
   /// adds a token to a result while updating the score and avoiding putting several unknown in a row
   let addToken token ((knownCount,length),pattern) =
      match token, pattern with
      | Unknown, Unknown::_ -> (knownCount,length), pattern
      | Unknown, _ -> (knownCount,length+1), Unknown::pattern
      | _ -> (knownCount+1,length+1), token::pattern
   /// returns the best of two results
   let best (score1,pattern1) (score2,pattern2) =
      let n1,l1 = score1
      let n2, l2 = score2
      if (n1,-l1) > (n2,-l2) then (score1,pattern1) else (score2,pattern2)
   /// find the fther of two patterns and its score
   let rec father followsUnknow depth1 pattern1 depth2 pattern2 =
      if memory.ContainsKey((depth1,depth2,followsUnknow)) then memory.[(depth1,depth2,followsUnknow)] else
         match pattern1, pattern2 with
         | [], [] -> (0,0), []
         | [], _ | _, [] -> (0,1), [Unknown]
         | t1::q1, t2::q2 when (t1 <> t2) && (not followsUnknow) ->
            // mismatch, we are forced to drop both
            father true (depth1+1) q1 (depth2+1) q2 |> addToken Unknown
            |> memorise depth1 depth2 followsUnknow
         | t1::q1, t2::q2 when (t1 <> t2) (*followsUnknow*) ->
            // mismatch after a drop, we can drop anyone of them
            let drop1 = father true (depth1+1) q1 depth2 pattern2 |> addToken Unknown
            let drop2 = father true depth1 pattern1 (depth2+1) q2 |> addToken Unknown
            best drop1 drop2
            |> memorise depth1 depth2 followsUnknow
         | t1::q1, t2::q2 when (*t1=t2*) (not followsUnknow) ->
            // match, we can keep or drop both
            let keep = father (t1=Unknown) (depth1+1) q1 (depth2+1) q2 |> addToken t1
            let drop = father true (depth1+1) q1 (depth2+1) q2 |> addToken Unknown
            best keep drop
            |> memorise depth1 depth2 followsUnknow
         | t1::q1, t2::q2 (*t1=t2 && followsUnknow*) ->
            // match after a drop, we can keep or drop any of them
            let keep = father (t1=Unknown) (depth1+1) q1 (depth2+1) q2 |> addToken t1
            let drop1 = father true (depth1+1) q1 depth2 pattern2 |> addToken Unknown
            let drop2 = father true depth1 pattern1 (depth2+1) q2 |> addToken Unknown
            best (best keep drop1) drop2
            |> memorise depth1 depth2 followsUnknow
   let score, pattern = father false 0 pattern1 0 pattern2
   pattern

/// takes a pattern and outputs a sequence of all the patterns that matches the current pattern
/// WARNING : complexity 2^length(pattern)
let rec allCommonPatterns pattern =
   match pattern with
   | [] -> [[]]
   | token :: pattern -> 
      let fathers = allCommonPatterns pattern
      List.collect (addToken token) fathers

//-----------------------------------------------------------------------------
// TREE

/// represents a tree of patterns
type PatternTree = {pattern : Pattern; childrens : PatternTree list}

/// assemble a tree
let makePatternTree childrens pattern = {pattern=pattern; childrens=childrens}

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