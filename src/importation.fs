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
   let wordRegexp = "([0-9+-\.]+|[\w_]+|[\s]+|\p{P}|\p{S})"
   Regex.Split(log, wordRegexp)
   |> Seq.filter ((<>) "")
   |> Seq.map Word
   |> Seq.toList

/// takes a path and outputs a list of patterns
let importLogs path =
   System.IO.File.ReadLines path
   |> Seq.map tokeniser
   |> Seq.distinct
   |> Seq.toList