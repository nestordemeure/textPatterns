module Importation

open Pattern
open System.Text.RegularExpressions

//-----------------------------------------------------------------------------
// PREPROCESSING

/// takes a log and outputs a list of tokens by cutting the log into :
/// - groups of digits
/// - groups of letters (including digits and '_')
/// - groups of whitespaces
/// - a punctuation sign
/// - a special punctuation sign
/// - a group of other characters
let tokeniser (log:string) =
   let wordRegexp = "([\w_]+|[\s]+|\p{P}|\p{S})"
   Regex.Split(log, wordRegexp)
   |> Seq.filter ((<>) "")
   |> Seq.map Word
   |> Seq.toList

//-----------------------------------------------------------------------------
// TYPE DEDUCTION

/// returns true if a char is a digit
let isDigit c = 
   (c >= '0') && (c <= '9')

/// set that contains all the months in their abreviated format
let months = set ["Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"]

/// returns true if c is a space
let isSpace c =
   (c = ' ') || (c = '\t') || (c = '\n')

/// returns true if c is one of the chars found in an hexadecimal number
let isInHexa c =
   (isDigit c) || (c = 'x') || ((c >= 'a') && (c <= 'f')) || ((c >= 'A') && (c <= 'F'))

//-----

/// active pattern that matches integers
let (|Integer|_|) token =
   match token with
   | Word w when String.forall isDigit w -> Some ()
   | NumberInteger -> Some ()
   | _ -> None

/// active pattern that matches integers lower than a given int
let (|IntMax|_|) maximumInt token =
   match token with
   | Word w when (String.forall isDigit w) && (int w <= maximumInt) -> Some ()
   | _ -> None

/// active pattern that matches months
let (|MonthMatcher|_|) token =
   match token with
   | Word w when Set.contains w months -> Some ()
   | _ -> None

/// active pattern that matches spaces
let (|Spaces|_|) token =
   match token with
   | Word w when String.forall isSpace w -> Some ()
   | Space -> Some ()
   | _ -> None

/// active pattern that matches the tail of hexadecimal numbers
let (|Hexa|_|) token =
   match token with
   | Word w when w.StartsWith("0x") && String.forall isInHexa w -> Some w.Length
   | _ -> None

/// active pattern that matches all kind of separators
/// anything that is not a number or a letter ?
let (|Separator|_|) token =
   match token with
   | Word ":" | Spaces | Word "-" | Word "~" | Word "." | Word "/" 
   | Word "(" | Word ")" | Word "[" | Word "]" | Word "\"" | Word "\'" 
   | Word "," | Word "`" | Word "=" | Word "#" | Word "+" | Word "*" | Word "^" -> Some token
   | _ -> None

let typeSeparator token =
   match token with
   | Spaces -> Space
   | _ -> token

/// matches elements that ends with digits and rewrite them
let NumberedElement token =
   token

/// matches a path
let (|PathMatcher|_|) tokens =
   /// extracts the path that was started
   let rec extractPath tokens =
      match tokens with
      | [] -> false, []
      | Word "/" :: q -> true, snd (extractPath q) // this separator proves that we are in a path
      | Word "." :: q | Word "-" :: q | Word ":" :: q -> extractPath q // those separator are allowed in a path
      | Separator _ :: _ -> false, tokens // any other separator marks the end of the path
      | _ :: q -> extractPath q
   match tokens with
   | Word "/" :: q -> 
      let (isPath,q) = extractPath q
      if isPath then Some q else None
   | _ -> None

/// types a string of token
/// we do not directly type integers and floats for fear of a mistake
let rec typeDeduction tokens =
   match tokens with
   | [] -> []
   | MonthMatcher :: q -> Month :: typeDeduction q
   | Word "[" :: Spaces :: Integer :: Word "." :: Integer :: Word "]" :: q 
   | Word "[" :: Integer :: Word "." :: Integer :: Word "]" :: q -> LPID :: typeDeduction q
   | Hexa n ::q -> NumberHexadecimal n :: typeDeduction q
   | Spaces :: q -> Space :: typeDeduction q
   | PathMatcher q -> Path :: typeDeduction q
   | Integer :: q -> NumberInteger :: typeDeduction q
   | t::q -> t :: typeDeduction q

//-----------------------------------------------------------------------------
// IMPORTATION

/// takes a path and outputs a list of patterns
let importLogs path =
   System.IO.File.ReadLines path
   |> Seq.map (tokeniser >> typeDeduction)
   |> Seq.distinct
   |> Seq.toList