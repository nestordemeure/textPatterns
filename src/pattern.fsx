
/// represents a token
type Token =
   | Word of string
   | Unknown

/// represents a pattern 
type Pattern = Token list

//-----------------------------------------------------------------------------
// PATTERN

/// the least specific pattern, ever
let universalPattern = [Unknown]

/// returns true if the list of tokens matches the pattern
let rec matchPattern pattern tokens =
   match pattern, tokens with
   | [], [] -> true
   | Unknown :: newPattern, _ :: newTokens ->
      (matchPattern newPattern newTokens) // match 1 token (redondant but usefull to express match with empty list)
      || (matchPattern pattern newTokens) // match 1+ tokens
      || (matchPattern newPattern tokens) // match 0 token
   | p :: pattern, t :: tokens when p = t -> 
      matchPattern pattern tokens
   | _ -> false

//-----------------------------------------------------------------------------
// SCORING

/// represents the evalutation of a pattern
/// specificity is (length-unknownCount)/length
/// Note : the type is layed out so that comparaison will be by specificity and then length
type Score = 
   {specificity : float; length : int; unknownCount : int}

/// returns the score of a pattern
let score pattern =
   let mutable length = 0
   let mutable unknownCount = 0
   let count t =
      length <- length + 1
      if t = Unknown then unknownCount <- unknownCount + 1
   List.iter count pattern
   let specificity = float (length - unknownCount) / float length
   {specificity=specificity; length=length; unknownCount=unknownCount}

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
/// TODO we could gain some speed by propagating the scores in order to avoid recomputing them at each iterations
/// TODO could be greatly speed up with memoization/dynamic programming
let rec commonPattern pattern1 pattern2 =
   match pattern1, pattern2 with
   | [], [] -> []
   | [], _ | _, [] -> [Unknown]
   | t1::q1, t2::q2 when t1 = t2 ->
      t1 :: commonPattern q1 q2
   | _::q1, _::q2 ->
      let drop1 = commonPattern q1 pattern2 |> addUnknown
      let drop2 = commonPattern q2 pattern1 |> addUnknown
      if score drop1 > score drop2 then drop1 else drop2

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