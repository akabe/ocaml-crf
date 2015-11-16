open Format

type ('a, 'b) t =
  | Ref of 'a
  | Node of 'a * 'b * ('a, 'b) t list
