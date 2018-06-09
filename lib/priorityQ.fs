// PAIRING HEAP : AMORTIZED MAX PRIORITY QUEUE
/// good amortized Max Priority Queue implemented using a PairingHeap
/// the price of a deleteMin/popMin can go up to O(n) but it is amortized into O(ln(n)) other operations are O(1)
[<RequireQualifiedAccess>]
module PriorityQueue


/// struct to store a value and its key (cache friendly)
type HeapEntry<'K,'V> = struct val k:'K val v:'V new(k,v) = {k=k;v=v} end

/// good amortized Min Priority Queue
/// the price of a deleteMin/popMin can go up to O(n) but it is amortizd into O(ln(n)) other operations are O(1)
type PairingHeap<'K,'V> =
   | EmptyHeap
   | Heap of HeapEntry<'K,'V> * (PairingHeap<'K,'V> list)
 
let empty = EmptyHeap

let inline singleton k v = Heap( HeapEntry(k,v) , [])

let inline isEmpty pq = pq = EmptyHeap

/// Return number of elements in the priority queue. 
/// WARNING : 0(n)
let rec size pq =
   match pq with 
   | EmptyHeap -> 0 
   | Heap(kv,l) -> 1 + (List.sumBy size l)

let merge pq1 pq2 =
   match pq1, pq2 with 
   | EmptyHeap, _ -> pq2
   | _, EmptyHeap -> pq1
   | Heap(kv1,l1), Heap(kv2,l2) -> if kv1 > kv2 then Heap(kv1, pq2 :: l1) else Heap(kv2, pq1 :: l2)

/// merge a list of heaps using a pairing procedure
/// non tail-recursive version, efficient in practice, based on Okazaki's implementation
let rec mergeMany l =
   match l with 
   | [] -> EmptyHeap
   | [pq] -> pq
   | pq1::pq2::q -> merge (merge pq1 pq2) (mergeMany q)

let push k v pq = merge pq (singleton k v)

let deleteHead pq = 
   match pq with 
   | EmptyHeap -> pq 
   | Heap(_,l) -> mergeMany l

let peek pq =
   match pq with 
   | EmptyHeap -> None
   | Heap(kv,_) -> Some(kv.k, kv.v)

let pop pq = 
   match pq with 
   | EmptyHeap -> None 
   | Heap(kv,l) -> Some( (kv.k,kv.v), mergeMany l)

let unsafePop pq =
   match pq with
   | EmptyHeap -> failwith "The Priority Queue was Empty" 
   | Heap(kv,l) -> (kv.k,kv.v), mergeMany l

let fromList l = l |> List.map (fun (k,v) -> singleton k v) |> mergeMany

let fromSeq sq = [for (k,v) in sq -> singleton k v] |> mergeMany

let toSeq pq = Seq.unfold pop pq