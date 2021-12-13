type t = {
  buffer : bytes;
  get : int -> Uint8.t;
  set : int -> Uint8.t -> unit;
}

exception Out_of_range of string

let make ?buffer ~name (b, e) =
  let get buffer addr =
    try
      Bytes.unsafe_get buffer (addr - b)
    with
    | Invalid_argument _ ->
      let err =
        Printf.sprintf "tried to get byte at address %d in %s (%d to %d)"
          addr name b e
      in
      raise (Out_of_range err)
  in
  let set buffer addr v =
    try
      Bytes.set buffer (addr - b) v
    with
    | Invalid_argument _ ->
      let err =
        Printf.sprintf "tried to set byte at address %d in %s (%d to %d)"
          addr name b e
      in
      raise (Out_of_range err)
  in
  let buffer =
    match buffer with
    | Some buffer -> buffer
    | _ -> Bytes.make (e - b + 1) '\000'
  in
  let get = get buffer in
  let set = set buffer in
  { buffer; set; get; }

let void () = {
  buffer = Bytes.empty;
  get = (fun _ -> Uint8.inj 0xFF);
  set = (fun _ _ -> ());
}
