
//-----------------------------------------------------------------------------
// MAIN

/// takes a path, displays the tree builded from the associated logs
let main path =
   let logs = Importation.importLogs path |> List.take 500
   let tree = Hierarchical.buildTree logs
   Output.displayTree tree

//-----------------------------------------------------------------------------
// TESTS

printfn "Starting..."

//main "tests/kittens.txt"
//main "tests/log.txt"
main "tests/kern.log" // taken from /var/log/kern.log