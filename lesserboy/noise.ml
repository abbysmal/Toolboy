(*
       Noise
  FF1F ---- ---- Not used
  NR41 FF20 --LL LLLL Length load (64-L)
  NR42 FF21 VVVV APPP Starting volume, Envelope add mode, period
  NR43 FF22 SSSS WDDD Clock shift, Width mode of LFSR, Divisor code
  NR44 FF23 TL-- ---- Trigger, Length enable

*)
open Machine.Noise

let pred_z z = if z = 0 then 0 else pred z
let pred_z' z = if z = Uint8.zero then Uint8.zero else Uint8.pred z

let divisor_code = function
  | 0 -> 8
  | 1 -> 16
  | 2 -> 32
  | 3 -> 48
  | 4 -> 64
  | 5 -> 80
  | 6 -> 96
  | 7 -> 112
  | _ -> assert false

let trigger' t =
  t.enabled <- true;
  if t.length_counter = 0 then
    t.length_counter <- 64;
  t.timer <- (divisor_code (Uint8.proj t.divisor_code)) lsl (Uint8.proj t.clock_shift);
  t.envelope_timer <- t.envelope_period;
  t.volume <- Uint8.proj t.starting_volume;
  t.lsfr <- 0x7FFF

let step (t : t) =
  t.timer <- pred t.timer;
  if t.timer <= 0 then begin
    let b = (t.lsfr land 1) lxor ((t.lsfr land 0b10) lsr 1) in
    t.lsfr <- t.lsfr lsr 1;
    if b = 1 then
      t.lsfr <- t.lsfr lor 0x4000;
    if t.width_mode then
      t.lsfr <- (t.lsfr land (lnot 0x40)) lor (if b = 1 then 0b100000 else 0);
    t.timer <- (divisor_code (Uint8.proj t.divisor_code)) lsl (Uint8.proj t.clock_shift)
  end

let length_tick (t : t) =
  t.length_counter <- pred_z t.length_counter;
  if t.length_counter = 0 && t.length_enabled then
    t.enabled <- false

let env_tick (t : t) =
  if t.envelope_period > Uint8.zero then begin
    t.envelope_timer <- pred_z' t.envelope_timer;
    if t.envelope_timer >= Uint8.zero then begin
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

let set t v a =
  match a with
  | 0xFF1F -> ()
  | 0xFF20 ->
    t.length_load <- Uint8.(v land inj 0b11111);
    t.length_counter <- 64 - Uint8.proj t.length_load;
  | 0xFF21 ->
    t.starting_volume <- Uint8.((v lsr inj 4) land inj 0xF);
    t.volume <- Uint8.proj t.starting_volume;
    t.env_add_mode <- Uint8.(v land inj 0x8) = Uint8.inj 0x8;
    t.period <- Uint8.(v land inj 0x7);
    t.envelope_period <- t.period;
  | 0xFF22 ->
    t.clock_shift <- Uint8.((v lsr inj 4) land inj 0xF);
    t.width_mode <- Uint8.((v lsr inj 3) land one) = Uint8.one;
    t.divisor_code <- Uint8.(v land inj 0b111)
  | 0xFF23 ->
    t.length_enabled <- Uint8.(v land inj 0x40) = Uint8.inj 0x40;
    t.trigger <- Uint8.((v lsr inj 7) land inj 0b1) = Uint8.one;
    if t.trigger then
      trigger' t;
  | _ -> assert false

let get t a =
  let open Uint8 in
  match a with
  | 0xFF1F -> zero
  | 0xFF20 -> t.length_load
  | 0xFF21 ->
    (t.starting_volume lsl inj 4)
    lor
    (if t.env_add_mode then inj 0b1000 else zero)
    lor
    t.period
  | 0xFF22 ->
    (t.clock_shift lsl inj 4)
    lor
    (if t.width_mode then inj 0b1000 else zero)
    lor
    t.divisor_code
  | 0xFF23 ->
    (if t.trigger then one else zero)
    lor
    ((if t.length_enabled then one else zero) lsl inj 6)
  | _ -> assert false

let sample t =
  if t.lsfr land 1 != 1 then t.volume else 0
