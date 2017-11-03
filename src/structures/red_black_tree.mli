type 'a set
val create: unit -> 'a set
val is_empty: 'a set -> bool
val size: 'a set -> int
val to_list: 'a set -> 'a list
val contains: 'a -> 'a set -> bool
val add: 'a -> 'a set -> 'a set
