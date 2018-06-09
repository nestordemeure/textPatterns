module Pattern

/// represents a token
type Token =
   | Word of string
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
/// NOTE uses memoization to reduce complexity to o(n²)
/// TODO dynamic programming could greatly increase performances
/// TODO we could gain some speed by propagating the scores in order to avoid recomputing them at each iterations
let commonPattern pattern1 pattern2 =
   let memory = System.Collections.Generic.Dictionary<int*int,int*int*Pattern>()
   /// add a token (fusing the unknown) and updates the score
   let addToken token (known,length,pattern) =
      match token,pattern with
      | Unknown, Unknown::q -> (known,length,pattern)
      | Unknown, _ -> (known,length+1,token::pattern)
      | _ -> (known+1,length+1,token::pattern)
   /// goes to the next occurence of the given token in the string
   let rec fastForwardTo depth pattern token =
      match pattern with
      | [] -> depth, pattern
      | t::q when t = token -> depth, pattern
      | t::q -> fastForwardTo (depth+1) q token
   /// returns (known,length,commonPattern)
   /// the common pattern covers one or more tokens (not zero) unless it comes at the end of the line
   let rec father followsUnknown depth1 pattern1 depth2 pattern2 =
      if memory.ContainsKey (depth1,depth2) then memory.[(depth1,depth2)] else
         match pattern1, pattern2 with
         | [], [] -> 
            0,0,[]
         | [], _ | _, [] -> 
            0,1,universalPattern
         | t1::q1, t2::q2 when t1 = t2 ->
            father (t1=Unknown) (depth1+1) q1 (depth2+1) q2 |> addToken t1
         | t1::q1, t2::q2 when not followsUnknown ->
            father true (depth1+1) q1 (depth2+1) q2 |> addToken Unknown
         | t1::q1, t2::q2 (*when followsUnknown*) ->
            let (n1,l1,keep1) = // t1 is part of the pattern
               let (depth2,pattern2) = fastForwardTo depth2 pattern2 t1
               father followsUnknown depth1 pattern1 depth2 pattern2
            let (n2,l2,drop1) = // t1 is not part of the pattern
               father true (depth1+1) q1 depth2 pattern2
            let result = if (n1,-l1) > (n2,-l2) then (n1,l1,keep1) else (n2,l2,drop1)
            memory.[(depth1,depth2)] <- result
            result
   let (known,length,pattern) = father false 0 pattern1 0 pattern2
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