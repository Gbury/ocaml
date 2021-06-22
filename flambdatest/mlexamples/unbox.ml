
type 'a ref = { mutable contents : 'a }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( + ) : int -> int -> int = "%addint"
external ( +. ) : float -> float -> float = "%addfloat"
external raise : exn -> 'a = "%raise"
external raise_notrace : exn -> 'a = "%raise_notrace"

exception Exit3 of int option

let f5 x g =
  let r = ref x in
  try
    while !r < 42 do
      if
        try g !r
        with _ -> false
      then begin
        if !r < 4 then raise_notrace (Exit3 (Some (!r + 2)))
        else raise_notrace (Exit3 None)
      end;
      r := !r + 1
    done;
    0
  with (Exit3 x) ->
    match x with
    | None -> 4
    | Some x -> x + 5

