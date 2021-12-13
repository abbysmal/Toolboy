open Machine.Square

  (*
     NR10 FF10 -PPP NSSS Sweep period, negate, shift
     NR11 FF11 DDLL LLLL Duty, Length load (64-L)
     NR12 FF12 VVVV APPP Starting volume, Envelope add mode, period
     NR13 FF13 FFFF FFFF Frequency LSB
     NR14 FF14 TL-- -FFF Trigger, Length enable, Frequency MSB
  *)

let pred_z z = if z = 0 then 0 else pred z
let pred_z' z = if z = Uint8.zero then Uint8.zero else Uint8.pred z

let sweep_period nr10 = Uint8.((nr10 lsr (inj 4)) land (inj 0b111))
let sweep_negate nr10  = Uint8.((nr10 lsr (inj 3)) land (inj 0b1))
let sweep_shift nr10 = Uint8.(nr10 land (inj 0b111))

let duty nr11 = Uint8.((nr11 lsr (inj 6)) land (inj 0b11))
let length_load nr11 = Uint8.(nr11 land (inj 0b111111))
let starting_volume nr12 = Uint8.((nr12 lsr inj 4) land inj 0b1111)
let env_add_mode nr12 = Uint8.((nr12 lsr inj 3) land inj 0b1)
let period nr12 = Uint8.(nr12 land inj 0b111)

let frequency_lsb nr13 = nr13

let trigger nr14 = Uint8.((nr14 lsr inj 7) land inj 0b1)
let length_enable nr14 = Uint8.((nr14 lsr inj 6) land inj 0b1)
let frequency_msb nr14 = Uint8.(nr14 land inj 0b111)


  (*
     duty lookup table:
     Duty   Waveform    Ratio
     -------------------------
     0      00000001    12.5%
     1      10000001    25%
     2      10000111    50%
     3      01111110    75%
  *)

let duty_cycles = [|
  [| false; false; false; false; false; false; false; true; |];
  [| true; false; false; false; false; false; false; true; |];
  [| true; false; false; false; false; true; true; true; |];
  [| false; true; true; true; true; true; true; false; |];
|]

let get t a =
  let open Uint8 in
  match a with
  | 0xFF15 -> Uint8.zero
  | 0xFF10 ->
    t.sweep_period lsl inj 4
    lor
    (if t.sweep_negate then one else zero) lsl inj 3
    lor
    t.sweep_shift
  | 0xFF16
  | 0xFF11 ->
    t.duty lsl inj 6
    lor
    t.length_load
  | 0xFF17
  | 0xFF12 ->
    t.starting_volume lsl inj 4
    lor
    (if t.env_add_mode then one else zero) lsl inj 3
    lor
    t.period
  | 0xFF18
  | 0xFF13 -> t.frequency_lsb
  | 0xFF19
  | 0xFF14 ->
    t.trigger lsl inj 7
    lor
    (if t.length_enabled then one else zero) lsl inj 6
    lor
    t.frequency_msb;
  | _ -> assert false

let reset _ =  ()

let compute_sweep t =
  let new_freq = ref 0 in
  new_freq := t.sweep_freq lsr Uint8.proj t.sweep_shift;
  if t.sweep_negate then
    new_freq := t.sweep_freq - !new_freq
  else
    new_freq := t.sweep_freq + !new_freq;
  if !new_freq > 2047 then
    t.enabled <- false;

  !new_freq

let trigger' t =
  t.enabled <- true;
  t.step <- 0;
  if t.length_counter <= 0 then
    t.length_counter <- 64;
  t.timer <- (2048 - t.frequency) * 4;

  t.envelope_timer <- t.envelope_period;
  t.volume <- Uint8.proj t.starting_volume;

  if t.channel = `SC1 then begin
    t.sweep_freq <- t.frequency;
    t.sweep_timer <- t.sweep_period;
    t.sweep_enable <- t.sweep_period > Uint8.zero || t.sweep_shift > Uint8.zero;

  if t.sweep_shift > Uint8.zero then
    compute_sweep t |> ignore
  end

let set t v a =
  match a with
  | 0xFF15 -> ()
  | 0xFF10 ->
    t.sweep_period <- Uint8.((v lsr inj 4) land inj 0x7);
    t.sweep_negate <- Uint8.((v land inj 0x8) = inj 0x8);
    t.sweep_shift <- Uint8.(v land inj 0x7)
  | 0xFF16
  | 0xFF11 ->
    t.duty <- Uint8.((v lsr inj 0x6) land inj 0x3);
    t.length_load <- Uint8.(v land inj 0x3F);
    t.length_counter <- 64 - (Uint8.proj t.length_load)
  | 0xFF17
  | 0xFF12 ->
    t.dac_enabled <- Uint8.(v land inj 0xF8) != Uint8.zero;
    t.starting_volume <- Uint8.((v lsr inj 4) land inj 0xF);
    t.env_add_mode <- Uint8.(v land inj 0x8) = Uint8.inj 0x8;
    t.period <- Uint8.(v land inj 0x7);
    t.envelope_period <- t.period;
    t.volume <- Uint8.proj t.starting_volume;
  | 0xFF18
  | 0xFF13 ->
    t.frequency_lsb <- frequency_lsb v;
    t.frequency <- (t.frequency land 0x700) lor Uint8.proj v
  | 0xFF19
  | 0xFF14 ->
    t.frequency <- (t.frequency land 0xFF) lor ((Uint8.proj v land 0x7) lsl 8);
    t.length_enabled <- Uint8.(v land inj 0x40) = Uint8.inj 0x40;
    t.frequency_msb <- frequency_msb v;
    if trigger v = Uint8.one then
      trigger' t;
  | _ -> assert false

let step (t : t) =
  t.timer <- pred t.timer;

  if t.timer == 0 then begin
    t.step <- (succ t.step) land 0x7;
    t.timer <- (2048 - t.frequency) * 4;
  end

let length_tick (t : t) =
  t.length_counter <- pred_z t.length_counter;
  if t.length_counter <= 0 && t.length_enabled then
    t.enabled <- false

let env_tick (t : t) =
  if t.envelope_period > Uint8.zero then begin
    t.envelope_timer <- pred_z' t.envelope_timer;
    if t.envelope_timer = Uint8.zero then begin
      if t.env_add_mode then (
        if t.volume < 0xF then
          t.volume <- succ t.volume;
      ) else (
        if t.volume > 0 then
          t.volume <- pred t.volume;
      );
    t.envelope_timer <- t.envelope_period
    end;
  end

let sweep_tick t =
  t.sweep_timer <- Uint8.(pred t.sweep_timer);
  if t.sweep_period > Uint8.zero then begin
    if t.sweep_enable && t.sweep_timer <= Uint8.zero then begin
      let freq = compute_sweep t in
      if freq <= 2047 && t.sweep_shift > Uint8.zero then begin
        t.sweep_freq <- freq;
        t.frequency <- freq;
        ignore @@ compute_sweep t
      end;
      t.sweep_timer <- t.sweep_period;
    end
  end

let sample t =
  if not t.enabled then 0 else begin
    let p = duty_cycles.(Uint8.proj t.duty).(t.step) in
    if p then t.volume else 0
  end
