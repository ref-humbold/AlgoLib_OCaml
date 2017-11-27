type 'a deque
val create: unit -> 'a deque
val is_empty: 'a deque -> bool
val get_front: 'a deque -> 'a
val get_back: 'a deque -> 'a
val push_front: 'a -> 'a deque -> 'a deque
val push_back: 'a -> 'a deque -> 'a deque
val pop_front: 'a deque -> 'a deque
val pop_back: 'a deque -> 'a deque
