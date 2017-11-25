type 'a heap
val create: unit -> 'a heap
val is_empty: 'a heap -> bool
val get: 'a heap -> 'a
val push: 'a -> 'a heap -> 'a heap
val pop: 'a heap -> 'a heap
val merge: 'a heap -> 'a heap -> 'a heap
