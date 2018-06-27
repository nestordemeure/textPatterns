module Functions

/// shuffles an array (in-place)
let arrayShuffle a =
   let rand = new System.Random()
   let swap (a: _[]) x y =
       let tmp = a.[x]
       a.[x] <- a.[y]
       a.[y] <- tmp
   Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a
   a

/// shuffles a list
/// NOTE : this function requires a convertion to an array type
let listShuffle l =
   l |> Array.ofList |> arrayShuffle |> List.ofArray

/// replaces x with y in the list
let rec replaceInList x y l =
   match l with
   | [] -> []
   | t::q when t=x -> y :: q
   | t::q -> t :: replaceInList x y q

/// returns the (best_element,leftover) according to a given scoring function
/// returns None if the list is empty
let listTryExtractMaxBy score l =
   let rec extract bestScore bestElement leftover l =
      match l with
      | [] -> bestElement, leftover
      | t :: q ->
         let s = score t
         if s > bestScore then extract s t (bestElement::leftover) q
         else extract bestScore bestElement (t::leftover) q
   match l with
   | [] -> None
   | t :: q -> extract (score t) t [] q |> Some

let extractMax score l =
   let rec extract bestScore bestElement leftover l =
      match l with
      | [] -> bestElement, leftover
      | t :: q -> 
         let s = score t
         if s > bestScore then extract s t (bestElement::leftover) q
         else extract bestScore bestElement (t::leftover) q
   match l with
   | [] -> failwith "not enough elements"
   | t::q -> extract (score t) t [] q