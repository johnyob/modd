include Core
include Modd_parsing.Ast_types
include Modd_typing

module Dynarray : sig
  type 'a t [@@deriving sexp]

  val create : unit -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val length : 'a t -> int
  val iter : 'a t -> f:('a -> unit) -> unit
  val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
  val pop_last : 'a t -> 'a option
  val remove_last : 'a t -> unit
  val add_last : 'a t -> 'a -> unit
  val capacity : 'a t -> int
  val fit_capacity : 'a t -> unit
end = struct
  type 'a t =
    { mutable length : int
    ; mutable arr : 'a Option_array.t
    }
  [@@deriving sexp]

  let default_capacity = 64
  let create () = { length = 0; arr = Option_array.create ~len:default_capacity }

  let get t i =
    match Option_array.get t.arr i with
    | Some v -> v
    | None -> raise_s [%message "Unexpected empty slot"]
  ;;

  let set t i x =
    try Option_array.set_some t.arr i x with
    | _ -> raise_s [%message "Unexpected empty slot"]
  ;;

  let length t = t.length

  let pop_last ({ arr; length } as t) =
    if length = 0
    then None
    else (
      let last = length - 1 in
      (* We know [length > 0] so [last >= 0]. *)
      match Option_array.unsafe_get arr last with
      | None -> raise_s [%message "Missing element"]
      | Some x ->
        Option_array.unsafe_set_none arr last;
        t.length <- last;
        Some x)
  ;;

  let remove_last t =
    match pop_last t with
    | None -> raise_s [%message "Cannot remove last from empty array"]
    | Some _ -> ()
  ;;

  let capacity t = Option_array.length t.arr
  let next_capacity n = min (max 8 (n * 2)) Sys.max_array_length

  let ensure_capacity t capacity' =
    let capacity = capacity t in
    if capacity' < 0
    then raise_s [%message "Negative capacity"]
    else if capacity >= capacity'
    then ()
    else (
      if capacity' > Sys.max_array_length
      then raise_s [%message "Capacity exceeds max array length"];
      let next_capacity = max (next_capacity capacity) capacity' in
      let new_arr = Option_array.create ~len:next_capacity in
      Option_array.blit ~src:t.arr ~src_pos:0 ~dst:new_arr ~dst_pos:0 ~len:t.length;
      t.arr <- new_arr)
  ;;

  let ensure_extra_capacity t extra_capacity =
    ensure_capacity t (t.length + extra_capacity)
  ;;

  let add_last_if_room t x =
    let length = t.length in
    if length >= Option_array.length t.arr
    then false
    else (
      t.length <- length + 1;
      Option_array.unsafe_set_some t.arr length x;
      true)
  ;;

  let add_last t x =
    if add_last_if_room t x
    then ()
    else (
      let rec grow_and_add t elem =
        ensure_extra_capacity t 1;
        if not (add_last_if_room t elem) then grow_and_add t elem
      in
      grow_and_add t x)
  ;;

  let fit_capacity t =
    if capacity t = t.length
    then ()
    else t.arr <- Option_array.sub t.arr ~pos:0 ~len:t.length
  ;;

  let iter t ~f =
    for i = 0 to t.length - 1 do
      f (Option_array.unsafe_get_some_exn t.arr i)
    done
  ;;

  let iteri t ~f =
    for i = 0 to t.length - 1 do
      f i (Option_array.unsafe_get_some_exn t.arr i)
    done
  ;;
end
