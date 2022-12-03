open Machine.Wave

(*
  NR30 FF1A E--- ---- DAC power
  NR31 FF1B LLLL LLLL Length load (256-L)
  NR32 FF1C -VV- ---- Volume code (00=0%, 01=100%, 10=50%, 11=25%)
  NR33 FF1D FFFF FFFF Frequency LSB
  NR34 FF1E TL-- -FFF Trigger, Length enable, Frequency MSB
*)

let trigger' t =
  t.enabled <- true;
  if t.length_counter <= 0 then
    t.length_counter <- 256;
  t.timer <- (2048 - t.frequency) * 2;
  t.pos <- 0

let pred_z z = if z = 0 then 0 else pred z

let length_tick (t : t) =
  t.length_counter <- pred_z t.length_counter;
  if t.length_counter = 0 && t.length_enable then
    t.enabled <- false

let frequency_msb nr14 = Uint8.(nr14 land inj 0b111)

let step (m : Machine.t) =
  let t = m.sc3 in
  t.timer <- pred t.timer;
  if t.timer <= 0 then begin
    t.timer <- (2048 - t.frequency) * 2;
    t.pos <- succ t.pos;
    t.pos <- t.pos land 0x1F;
    let sample = Bytes.get t.wave_table t.pos in
    t.sample_buffer <- sample;
  end

external ( <= ) : int -> int -> bool = "%lessequal"
external ( >= ) : int -> int -> bool = "%greaterequal"

let is_between' v (a, b) = v >= a && v <= b [@@inline]

let set t v a =
  match a with
  | 0xFF1A ->
    t.dac_enabled <- Uint8.(v lsr inj 7) = Uint8.one
  | 0xFF1B ->
    t.length_load <- v
  | 0xFF1C ->
    let shift = [|4;0;1;2|].(((Uint8.proj v lsr 5) land 3)) in
    t.volume_code <- Uint8.inj shift
  | 0xFF1D ->
    t.frequency_lsb <- v;
    t.frequency <- (t.frequency land 0x700) lor Uint8.proj v;
  | 0xFF1E ->
    t.frequency <- (t.frequency land 0xFF) lor ((Uint8.proj v land 0x7) lsl 8);
    t.length_enable <- Uint8.(v land inj 0x40) = Uint8.inj 0x40;
    t.frequency_msb <- Uint8.(v land inj 0b111);
    t.trigger <- Uint8.((v lsr inj 7) land inj 0b1) = Uint8.one;
    if t.trigger then
      trigger' t;
  | a when is_between' a Machine.Audio.wave_table_range ->
    let pos = (a - 0xFF30) * 2 in
    Bytes.set t.wave_table pos Uint8.(v lsr inj 4);
    Bytes.set t.wave_table (pos + 1) Uint8.(v land inj 0x0F);
  | _ -> assert false

let get t a =
  match a with
  | 0xFF1A -> if t.dac_enabled then Uint8.inj 0b1000_0000 else Uint8.zero
  | 0xFF1B -> t.length_load
  | 0xFF1C -> Uint8.(t.volume_code lsl inj 5)
  | 0xFF1D -> t.frequency_lsb
  | 0xFF1E -> Uint8.(
    (if t.trigger then one else zero)
    lor
    (if t.length_enable then one else zero) lsl inj 6
    lor
    t.frequency_msb
  )
  | a when is_between' a Machine.Audio.wave_table_range ->
    let open Uint8 in
    let pos = (a - 0xFF30) * 2 in
    ((Bytes.get t.wave_table pos) lsl inj 4)
    lor
    ((Bytes.get t.wave_table (pos + 1)))
  | _ -> assert false

let sample t =
  if not t.enabled && not t.dac_enabled then 0 else begin
    Uint8.proj t.sample_buffer lsr Uint8.proj t.volume_code
  end
