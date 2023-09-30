include Core
include Modd_parsing

let post_incr r =
  let result = !r in
  Int.incr r;
  result
;;
