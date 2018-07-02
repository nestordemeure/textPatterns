open Pattern

//-----------------------------------------------------------------------------
// MAIN

/// takes a path, displays the tree builded from the associated logs
let main path =
   printfn "Loading Logs..."
   let logs = Importation.importLogs path
   printfn "Building tree..."
   let tree = Prefix.buildTree 0 logs
   Output.displayTree tree
   printfn "Cutting tree..."
   let patterns = Common.split tree
   //let patterns = Flatten.flatten tree
   List.iter (Output.stringOfPattern >> printfn "%s") patterns

//-----------------------------------------------------------------------------
// TESTS

printfn "Starting..."

//main "tests/kittens.txt"
//main "tests/log.txt"
main "tests/kern.log" // taken from /var/log/kern.log
