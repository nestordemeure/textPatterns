open Pattern

//-----------------------------------------------------------------------------
// MAIN

/// takes a path, displays the tree builded from the associated logs
let main path =
   let logs = Importation.importLogs path //|> List.take 700
   //let tree = Hierarchical.buildTree logs
   let tree = Incremental.buildTreeIncrementaly logs
   Output.displayTree tree

//-----------------------------------------------------------------------------
// TESTS

printfn "Starting..."

//main "tests/kittens.txt"
//main "tests/log.txt"
main "tests/kern.log" // taken from /var/log/kern.log
(*
let log1 = Importation.tokeniser "Jun  7 23:48:22 nestor-HP-ProBook-650-G1 kernel: [    3.072057] usb usb1: Product: EHCI Host Controller"
let log2 = Importation.tokeniser "Jun  7 23:48:22 nestor-HP-ProBook-650-G1 kernel: [    3.110518] AppArmor: AppArmor sha1 policy hashing enabled"

let pattern = Pattern.commonPattern log1 log2

let tree = {pattern=pattern; childrens = List.map (makePatternTree []) [log1;log2]}

Output.displayTree tree
*)