module I = Cartridge_internals

external ( <= ) : int -> int -> bool = "%lessequal"
external ( >= ) : int -> int -> bool = "%greaterequal"

let is_between v a b = v >= a && v <= b [@@inline]

type impl = {
  get : int -> Uint8.t;
  put : int -> Uint8.t -> unit;
  dump_sram : unit -> Bytes.t option;
  rom_bank : unit -> int option;
  ram_bank : unit -> int option;
}

let make_void () = {
  put = (fun _ _ -> ());
  get = (fun _ -> Uint8.zero);
  rom_bank = (fun () -> None);
  ram_bank = (fun () -> None);
  dump_sram = (fun () -> None);
}

let make_mbc0 t =
  let get = function
    | addr when is_between addr 0x0000 0x7FFF -> Bytes.get t addr
    | addr when is_between addr 0xA000 0xBFFF -> Uint8.zero
    | _ -> assert false
  in
  let put _ _ = () in
  let rom_bank () =  Some 0 in
  let ram_bank () =  Some 0 in
  let dump_sram () = None in
  {
    get;
    put;
    rom_bank;
    ram_bank;
    dump_sram;
  }

let make_mbc1 ?sav rom =
  let ram_size = I.ram_size rom in
  let rom_size = Bytes.length rom in
  let ram =
    match sav with
    | Some sav when Bytes.length sav = ram_size -> sav
    | Some _ ->
      let ram = Bytes.create ram_size in
      for i = 0 to (ram_size -1) do
        Bytes.set ram i '\255'
      done;
      ram
    | _ ->
      let ram = Bytes.create ram_size in
      for i = 0 to (ram_size -1) do
        Bytes.set ram i '\255'
      done;
      ram
  in
  let bank1 = ref 1 in
  let bank2 = ref 0 in
  let mode = ref false in
  let ram_enabled = ref false in

  let ram_offset' () =
    let bank = if !mode then !bank2 else 0x0 in
    0x2000 * bank in

  let ram_offset = ref 0 in

  let put addr v  =

    match addr with
    | addr when is_between addr 0x0000 0x1FFF ->
      if Uint8.proj v land  0b1111 = 0b1010 then
        ram_enabled := true
      else
        ram_enabled := false

    | addr when is_between addr 0x2000 0x3FFF ->
      bank1 :=
        if Uint8.proj v land 0b1_1111 == 0b0_0000 then
          0b0_0001
        else
          Uint8.proj v land 0b1_1111;

    | addr when is_between addr 0x4000 0x5FFF -> begin
        bank2 := Uint8.proj v land 0b11;
        ram_offset := ram_offset' ()
      end
    | addr when is_between addr 0x6000 0x7FFF -> begin
        mode := (Uint8.proj v land 0b1) == 0b1;
        ram_offset := ram_offset' ();
    end
    | addr when is_between addr 0xA000 0xBFFF -> begin
        match !ram_enabled with
        | false -> ()
        | true when !mode = false -> Bytes.set ram ((addr - 0xA000) land (Bytes.length ram - 1)) v
        | true ->
           let addr = (!ram_offset lor (addr land 0x1fff)) land (Bytes.length ram - 1) in
           Bytes.set ram addr v
      end
    | _ -> assert false
  in

  let get = function
    | addr when is_between addr 0x0000 0x3FFF -> Bytes.get rom addr
    | addr when is_between addr 0x4000 0x7FFF ->
      let rom_bank =
        let bank1 = if !bank1 = 0 then 1 else !bank1 in
        match !mode with
        | false -> ((!bank2 lsl 5) lor (bank1 land 0b11111))
        | true -> (0 lor (bank1 land 0b11111))
      in
      let addr' = (rom_bank lsl 14) lor (addr land 0b11111111111111)  in
      let addr' = addr' land (rom_size - 1) in
      Bytes.get rom addr'
    | addr when is_between addr 0xA000 0xBFFF -> begin
        match !ram_enabled with
        | false -> Uint8.inj 0xFF
        | true when !mode = false  ->
          Bytes.get ram ((addr - 0xA000) land (Bytes.length ram - 1))
        | true ->
          let addr = (!ram_offset lor (addr land 0x1fff)) land (Bytes.length ram - 1) in
          Bytes.get ram addr
    end
    | _ -> Uint8.inj 0xFF
  in

  let rom_bank () =  Some !bank1 in
  let ram_bank () =  Some !bank2 in

  let dump_sram () = Some ram in

  {
    get;
    put;
    rom_bank;
    ram_bank;
    dump_sram;
  }

let make_mbc3 ?sav rom =

  let ram_size = I.ram_size rom in
  let rom_size = Bytes.length rom in
  let ram =
    match sav with
    | Some sav when Bytes.length sav = ram_size -> sav
    | Some _ ->
      let ram = Bytes.create ram_size in
      for i = 0 to (ram_size -1) do
        Bytes.set ram i '\255'
      done;
      ram
    | _ ->
      let ram = Bytes.create ram_size in
      for i = 0 to (ram_size -1) do
        Bytes.set ram i '\255'
      done;
      ram
  in
  let ram_bank = ref 0x0 in
  let rom_bank = ref 0x0 in
  let ram_enabled = ref false in

  let put addr v  =

    match addr with
    | addr when is_between addr 0x0000 0x1FFF ->
      ram_enabled := Uint8.proj v land 0xF = 0xA
    | addr when is_between addr 0x2000 0x3FFF ->
      rom_bank := Uint8.proj v;
    | addr when is_between addr 0x4000 0x5FFF ->
      ram_bank := Uint8.proj v;
    | addr when is_between addr 0x6000 0x7FFF -> begin
        ()
        (* mode := (Uint8.proj v land 0b1) == 0b1;
         * ram_offset := ram_offset' (); *)
    end
    | addr when is_between addr 0xA000 0xBFFF -> begin
        match !ram_enabled with
        | false -> ()
        | true ->
          let addr = ((addr land 0x1FFF) + !ram_bank * 0x2000) land (ram_size - 1) in
          Bytes.set ram addr v
      end
    | _ -> assert false
  in

  let get = function
    | addr when is_between addr 0x0000 0x3FFF -> Bytes.get rom addr
    | addr when is_between addr 0x4000 0x7FFF ->
      let effective_address = (addr land 0x3FFF) + !rom_bank * 0x4000 in
      Bytes.get rom (effective_address land (rom_size - 1))
    | addr when is_between addr 0xA000 0xBFFF -> begin
        match !ram_enabled with
        | false -> Uint8.inj 0xFF
        | true ->
          let addr = ((addr land 0x1FFF) + !ram_bank * 0x2000) land (ram_size - 1) in
          Bytes.get ram addr
    end
    | _ -> Uint8.inj 0xFF
  in
  let rom_bank () = Some !rom_bank in
  let ram_bank () = Some !ram_bank in

  let dump_sram () = Some ram in

  {
    get;
    put;
    rom_bank;
    ram_bank;
    dump_sram;
  }

type t = {
  impl : impl;
}

exception Unimplemented of int

let make ?rom ?sav () =
  let impl =
    match rom with
    | None -> make_void ()
    | Some rom ->
      match I.cartridge_kind rom with
      | `Rom_only -> make_mbc0 rom
      | `Mbc1 -> make_mbc1 rom ?sav
      | `Mbc3 -> make_mbc3 rom ?sav
      | `Unimplemented d -> raise (Unimplemented d)
      | _ -> raise (Unimplemented 0x0)
  in
  { impl; }

let get { impl = { get; _ }; } = get

let put { impl = { put; _ }; } = put

let dump_sram { impl = { dump_sram; _ }; } = dump_sram

let rom_bank { impl = { rom_bank; _ }; } = rom_bank ()

let ram_bank { impl = { ram_bank; _ }; } = ram_bank ()
