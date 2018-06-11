module Importation

open Pattern
open System.Text.RegularExpressions

//-----------------------------------------------------------------------------
// PREPROCESSING

/// takes a log and outputs a list of tokens by cutting the log into :
/// - groups of digits (including '+', '-' and '.')
/// - groups of letters (including digits and '_')
/// - groups of whitespaces
/// - a punctuation sign
/// - a special punctuation sign
/// - a group of other characters
let tokeniser (log:string) =
   let wordRegexp = "([0-9]+|[\w_]+|[\s]+|\p{P}|\p{S})"
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
   (isDigit c) || (c = 'x') || ((c <= 'a') && (c >= 'f')) || ((c <= 'A') && (c >= 'F'))

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
let (|Month|_|) token =
   match token with
   | Word w when Set.contains w months -> Some ()
   | _ -> None

/// active pattern that matches spaces
let (|Spaces|_|) token =
   match token with
   | Word w when String.forall isSpace w -> Some ()
   | _ -> None

/// active pattern that matches the tail of hexadecimal numbers
let (|HexaTail|_|) token =
   match token with
   | Word w when w.StartsWith('x') && String.forall isInHexa w -> Some (w.Length - 1)
   | _ -> None

/// types a string of token
let rec typeDeduction tokens =
   match tokens with
   | [] -> []
   | Word w :: Word "=" :: Integer :: q -> Word w :: Word "=" :: typeDeduction (NumberInteger :: q)
   | Month :: Word "  " :: IntMax 31 :: Word " " :: IntMax 24 :: Word ":" :: IntMax 60 :: Word ":" :: IntMax 60 :: q -> Date :: typeDeduction q
   | Word "[" :: Spaces :: Integer :: Word "." :: Integer :: Word "]" :: q 
   | Word "[" :: Integer :: Word "." :: Integer :: Word "]" :: q -> LONGPID :: typeDeduction q
   | Word "[" :: Integer :: Word "]" :: Word ":" :: q -> PID :: typeDeduction q
   | Word "0" :: HexaTail n ::q -> NumberHexadecimal n :: typeDeduction q
   | Integer :: Word "." :: Integer :: q
   | Integer :: Word "." :: q -> NumberFloat :: typeDeduction q
   //| Integer :: q -> NumberInteger :: typeDeduction q
   | t::q -> t :: typeDeduction q

//-----------------------------------------------------------------------------
// IMPORTATION

/// takes a path and outputs a list of patterns
let importLogs path =
   System.IO.File.ReadLines path
   |> Seq.map (tokeniser >> typeDeduction)
   |> Seq.distinct
   |> Seq.toList