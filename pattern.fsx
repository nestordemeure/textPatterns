open System.Text.RegularExpressions

//-----------------------------------------------------------------------------
// PATTERNS

/// represents a token
/// TODO type deduction would let us add int, float, date, etc tokens which would speed up the overall algorithm
type Token =
   | Word of string
   | Unknown

/// represents a pattern 
type Pattern = Token list

/// represents a tree of tokens
type PatternTree = {pattern : Pattern; childrens : PatternTree list}

/// builds a tree
let treeOfPattern childrens pattern = 
   {pattern=pattern; childrens=childrens}

/// the less specific pattern, ever
let universalPattern = [Unknown]

/// returns a representation of the score of a pattern : number_of_unknown, length
let score pattern =
   let sum (unknownNum,length) t =
      match t with 
      | Unknown -> (unknownNum+1,length+1)
      | _ -> (unknownNum,length+1)
   List.fold sum (0,0) pattern

/// returns true if the first score is strictly bigger than the second score
/// it is bigger if it has a bigger specificity or, in case of equality, if it has a bigger length
/// specificity is (length - number_of_unknown) / length
let biggerScore (unknowNumber1,length1) (unknowNumber2,length2) =
   let spec1 = (length1 - unknowNumber1) * length2
   let spec2 = (length2 - unknowNumber2) * length1
   (spec1 > spec2) || ( (spec1=spec2) && (length1 > length2) )

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
// IMPORTATION

/// takes a log and outputs a list of tokens by cutting the log into
/// groupes of numbers
/// groups of letters
/// groups of whitespaces
/// a punctuation sign
/// a special punctuation sign
/// a group of other characters
let tokeniser (log:string) =
   let wordRegexp = "([0-9]+|[\w]+|[\s]+|\p{P}|\p{S})"
   Regex.Split(log, wordRegexp)
   |> Seq.filter ((<>) "")
   |> Seq.map Word
   |> Seq.toList

/// takes a log, converts it into a pattern and build a tree with no childrens around it
/// TODO we could had type deduction after the tokenisation
let treeOfLog log = 
   log
   |> tokeniser
   |> treeOfPattern []

/// takes a sequence of logs and outputs a sequence of patterns
/// can be done in parallel trivially
let importation logs = 
   logs
   |> Seq.map treeOfLog
   |> Seq.distinctBy (fun node -> node.pattern)
   |> Seq.toList

//-----------------------------------------------------------------------------
// TREE BUILDING

let addToken token pattern =
   match token, pattern with
   | Unknown, Unknown :: _ -> [pattern]
   | Unknown, _ -> [Unknown :: pattern]
   | _, Unknown :: _ -> [token :: pattern; pattern]
   | _ -> [token :: pattern; Unknown :: pattern]

/// takes a pattern and outputs a sequence of all the patterns that 
/// - matches the current pattern
/// - could have a score superior to the given score
let rec fatherPatterns minimumScore pattern =
   match pattern with
   | [] -> [[]]
   | token :: pattern -> 
      let fathers = fatherPatterns minimumScore pattern
      List.collect (addToken token) fathers

//-----

/// takes pattern and outputs a pattern that :
// - matches at least two logs
// - maximise score
let findBestPattern treeList =
   let bestPattern = universalPattern
   let bestScore = score bestPattern
   let candidates = Set.empty
   /// if this pattern is or could become better than the best known, it is stored
   let findBest (bestPattern, bestScore, candidates) pattern =
      let newScore = score pattern
      if biggerScore bestScore newScore then
         (bestPattern, bestScore, candidates)
      elif Seq.contains pattern candidates then
         let newCandidates = Set.remove pattern candidates
         (pattern, newScore, newCandidates)
      else
         let newCandidates = Set.add pattern candidates
         (bestPattern, bestScore, newCandidates)
   /// generate all potential father patterns and find the best one
   let findBestInTree (bestPattern, bestScore, candidates) tree =
      fatherPatterns bestScore tree.pattern
      |> Seq.fold findBest (bestPattern, bestScore, candidates)
   treeList
   |> List.fold findBestInTree (bestPattern, bestScore, candidates)
   |> fun (bestPattern,_,_) -> bestPattern

/// takes a list of logs and outputs a pattern tree
let buildTree logs =
   let treeList = importation logs
   let rec build treeList =
      match treeList with
      | [] -> treeOfPattern [] universalPattern
      | [tree] -> tree
      | _ ->
         let bestPattern = findBestPattern treeList
         let matched, nonMatched = List.partition (fun tree -> matchPattern bestPattern tree.pattern) treeList
         let newTree = treeOfPattern matched bestPattern
         build (newTree :: nonMatched)
   build treeList

//-----------------------------------------------------------------------------
// OUTPUT

/// takes a string and outputs a token
let stringOfToken token =
   match token with
   | Unknown -> "%s"
   | Word w -> w

/// prints the string associated with a pattern
let stringOfPattern pattern =
   pattern 
   |> List.map stringOfToken 
   |> List.reduce (+)

/// rough display of a tree
let stringOfTree tree =
   let mutable result = sprintf "->\"%s\"\n" (stringOfPattern tree.pattern)
   let rec displayTreeList indentation l =
      match l with
      | [] -> ()
      | [tree] -> 
         result <- result + sprintf "%s└─>\"%s\"\n" indentation (stringOfPattern tree.pattern)
         let newIndentation = indentation + "  "
         displayTreeList newIndentation tree.childrens
      | tree :: q -> 
         result <- result + sprintf "%s├─>\"%s\"\n" indentation (stringOfPattern tree.pattern)
         let newIndentation = indentation + "│ "
         displayTreeList newIndentation tree.childrens
         displayTreeList indentation q
   displayTreeList " " tree.childrens
   result

//-----------------------------------------------------------------------------
// TEST

// tests the tokeniser
let log = "paco is 33 toco. or is it 29hum.^^..?"
let pattern = tokeniser log
printfn "%A" pattern

// test match
let m = matchPattern universalPattern pattern
printfn "match? : %b" m

// tests the pattern extractor
let log1 = "I like kittens!"
let log2 = "I like dogs?"
let log3 = "I like cats!"
let log4 = "I like ham and jam."
let log5 = "I like ham but not jam."
let tree = buildTree [log1; log2; log3; log4; log5]
printfn "%s" (stringOfTree tree)