module Hybrid

/// build a tree using seedSize random elements to build a first tree with hierarchical clustering and then using an incremental approach
/// the method should provide a better tree structure and good speed
let buildTree seedSize logs =
   printfn "Hybrid clustering : splitting data..."
   let seedLogs,logs = if List.length logs <= seedSize then logs,[]
                       else logs |> Functions.listShuffle |> List.splitAt seedSize
   printfn "Hybrid clustering : building first tree..."
   let seedTree = Hierarchical.buildTree seedLogs
   printfn "Hybrid clustering : starting incremental build..."
   List.fold Incremental.addPatternIntoTree seedTree logs
   // with progress bar
   //let mutable i = 1
   //let imax = List.length logs |> float
   //List.fold (fun t p -> i <- i+1; printfn "Progress : %.2f percents" ((float i * 100.) / imax); Incremental.addPatternIntoTree t p) seedTree logs
