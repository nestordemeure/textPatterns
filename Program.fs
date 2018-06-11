open Pattern

//-----------------------------------------------------------------------------
// MAIN

/// takes a path, displays the tree builded from the associated logs
let main path =
   printfn "Loading Logs..."
   let logs = Importation.importLogs path
   printfn "Building tree..."
   //let logs = logs (*|> Functions.listShuffle*) |> List.take 100 //|> List.sort//By List.length
   //let tree = Hierarchical.buildTree logs // nearly optimal but does not scale
   //let tree = Incremental.buildTreeIncrementaly (Functions.listShuffle logs) // scales but not optimal
   //let tree = Hybrid.buildTree 200 logs
   let tree = Coarse.buildTree logs
   printfn "Printing tree..."
   Output.displayTree tree

//-----------------------------------------------------------------------------
// TESTS

printfn "Starting..."

//main "tests/kittens.txt"
//main "tests/log.txt"
main "tests/kern.log" // taken from /var/log/kern.log
