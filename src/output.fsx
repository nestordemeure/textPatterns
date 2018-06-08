
#load "pattern.fsx"
open Pattern

//-----------------------------------------------------------------------------
// STRING CONVERSION

/// takes a token and outputs a string
let stringOfToken token =
   match token with
   | Unknown -> "%s"
   | Word w -> w

/// returns the string associated with a pattern
let stringOfPattern pattern =
   pattern 
   |> List.map stringOfToken 
   |> List.reduce (+)

//-----------------------------------------------------------------------------
// DISPLAY

/// displays a tree
let displayTree tree =
   /// displays a list of trees
   let rec displayTreeList indentation l =
      match l with
      | [] -> ()
      | [tree] -> 
         printfn "%s└─>\"%s\"" indentation (stringOfPattern tree.pattern)
         let newIndentation = indentation + "  "
         displayTreeList newIndentation tree.childrens
      | tree :: q -> 
         printfn "%s├─>\"%s\"" indentation (stringOfPattern tree.pattern)
         let newIndentation = indentation + "│ "
         displayTreeList newIndentation tree.childrens
         displayTreeList indentation q
   printfn "->\"%s\"" (stringOfPattern tree.pattern)
   displayTreeList " " tree.childrens
