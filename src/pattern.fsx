
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
/// TODO memoize for better efficiency
let rec matchPattern pattern tokens =
   match pattern, tokens with
   | [], [] -> true
   | Unknown :: newPattern, _ :: newTokens ->
      (matchPattern newPattern newTokens) // match 1 token (redondant but usefull to express match with empty list)
      || (matchPattern pattern newTokens) // match 1+ tokens
      //|| (matchPattern newPattern tokens) // match 0 token : more prone to introducing non intuitive things than to solve problems
   | p :: pattern, t :: tokens when p = t -> 
      matchPattern pattern tokens
   | _ -> false

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
   let memory = System.Collections.Generic.Dictionary<int*int,Pattern>()
   let rec father (depth1,depth2) pattern1 pattern2 =
      if memory.ContainsKey (depth1,depth2) then memory.[(depth1,depth2)] else
         match pattern1, pattern2 with
         | [], [] -> []
         | [], _ | _, [] -> [Unknown]
         | t1::q1, t2::q2 when t1 = t2 ->
            t1 :: father (depth1+1,depth2+1) q1 q2 // TODO memoize
         | _::q1, _::q2 ->
            let drop1 = father (depth1+1,depth2) q1 pattern2 |> addUnknown
            let drop2 = father (depth1,depth2+1) pattern1 q2 |> addUnknown
            let result = if score drop1 > score drop2 then drop1 else drop2
            memory.[(depth1,depth2)] <- result
            result
   father (0,0) pattern1 pattern2

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