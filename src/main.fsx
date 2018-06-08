
#load "importation.fsx"
#load "clustering/hierarchical.fsx"
#load "output.fsx"

//-----------------------------------------------------------------------------
// MAIN

/// takes a path, displays the tree builded from the associated logs
let main path =
   let logs = Importation.importLogs path
   let tree = Hierarchical.buildTree logs
   Output.displayTree tree

//-----------------------------------------------------------------------------
// TESTS

printfn "Starting..."

main "tests/kittens.txt"
//main "tests/log.txt"
