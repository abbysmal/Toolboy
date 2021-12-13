open Cpu
open Machine

let incr_pc m = m.cpu.pc <- succ m.cpu.pc [@@inline] 
let incr_pc' m n = m.cpu.pc <- m.cpu.pc + n [@@inline] 
let get_pc m = Mmu.get m m.cpu.pc [@@inline]

let ld_n m reg =
  Mmu.get m m.cpu.pc |> put_register m reg;
  incr_pc m; 
  8

let ld_n_hl m =
  let addr = get_register_pair m HL in
  get_pc m |> Mmu.put m addr;
  incr_pc m;
  4

let ld_rr m reg1 reg2 =
  get_register m reg2 |>
  put_register m reg1;
  4

let ld_nn_sp m =
  let v = m.cpu.sp in
  let nn = Mmu.get_u16 m m.cpu.pc in
  Mmu.put_u16 m nn v;
  incr_pc' m 2;
  20

let ld_sp m =
  m.cpu.sp <- Mmu.get_u16 m m.cpu.pc; 
  incr_pc' m 2;
  12

let ld_p m pair reg =
  get_register_pair m pair |>
  Mmu.get m |>
  put_register m reg;
  8

let ld_0xFF00_r_ra m reg1 reg2 =
  let r1 = get_register m reg1 in
  let r2 = get_register m reg2 in
  Mmu.put m (0xFF00 + Char.code r2) r1;
  8

let ld_0xFF00_ra_r m reg1 reg2 =
  let r1 = get_register m reg1 in
  Mmu.get m (0xFF00 + Uint8.proj r1) |>
  put_register m reg2;
  8

let ld_0xFF00_r_n m reg =
  let n = get_pc m in
  get_register m reg |>
  Mmu.put m (0xFF00 + Uint8.proj n);
  incr_pc m;
  12

let ld_0xFF00_n_r m reg =
  let n = get_pc m in
  Mmu.get m (0xFF00 + Uint8.proj n) |>
  put_register m reg;
  incr_pc m;
  12

let xor_n m reg1 =
  let open Uint8 in
  let r1 = get_register m reg1 in
  let r2 = get_pc m in
  let res = r2 lxor r1 in
  put_flags m ~z:(res = Uint8.zero) ~n:false ~hc:false ~ca:false;
  put_register m reg1 res;
  incr_pc m;
  8

let xor_hl m reg1 =
  let open Uint8 in
  let r1 = get_register m reg1 in
  let r2 = get_register_pair m HL |> Mmu.get m in 
  let res = r2 lxor r1 in
  put_flags m ~z:(res = Uint8.zero) ~n:false ~hc:false ~ca:false;
  put_register m reg1 res;
  8

let xor m reg1 reg2 =
  let open Uint8 in
  let r1 = get_register m reg1 in
  let r2 = get_register m reg2 in
  let res = r2 lxor r1 in
  put_flags m ~z:(res = Uint8.zero) ~n:false ~hc:false ~ca:false;
  put_register m reg1 res;
  4

let ld_nn m pair =
  Mmu.get_u16 m m.cpu.pc |>
  put_register_pair m pair;
  incr_pc' m 2;
  12

let ld_hl_sp m =
  m.cpu.sp <- get_register_pair m HL;
  8

let ld_hl_sp_n m =
  let is_ca r1 r2 =
    let r1 = r1 land 0xFF in
    let r2 = r2 land 0xFF in
    r1 + r2 > 0xff
  in
  let is_hc r1 r2 =
    let r1 = r1 land 0xFF in
    let r2 = r2 land 0xFF in
    (r1 land 0xf) + (r2 land 0xf) > 0xf
  in
  let r2 = get_pc m in
  let n =
    if Uint8.proj r2 > 0x80 then
      -(0x80 - (Uint8.proj r2 - 0x80))
    else
      Uint8.proj r2
  in
  let v = m.cpu.sp + n in
  put_register_pair m HL v;
  let hc = is_hc m.cpu.sp n in
  let ca = is_ca m.cpu.sp n in
  put_flags m ~ca ~hc ~z:false ~n:false;
  incr_pc m;
  8

let ldd m reg pair =
  let hl = get_register_pair m pair in
  get_register m reg |>
  Mmu.put m hl;
  put_register_pair m HL (hl - 1);
  8

let ldd_hl m pair reg =
  let hl = get_register_pair m pair in
  Mmu.get m hl |>
  put_register m reg;
  put_register_pair m pair (hl - 1);
  8

let bit m reg bit =
  let r = get_register m reg in
  let z = not (Uint8.is_bit_set r bit) in
  put_flags m ~z ~hc:true ~n:false;
  8

let jr m flag cond =
  let imm = get_pc m in
  match is_flag_set flag m = cond with
  | true ->
    incr_pc' m (1 + (Uint8.to_signed imm));
    12
  | false ->
    incr_pc m;
    8

let jrz m flag =
  let imm = get_pc m in
  match is_flag_set flag m with
  | true ->
    incr_pc' m (1 + (Uint8.to_signed imm));
    12
  | false ->
    incr_pc m;
    8

let jr_n m =
  let imm = get_pc m in
  incr_pc' m (1 + (Uint8.to_signed imm));
  8

let jp_c m cond =
  let imm = Mmu.get_u16 m m.cpu.pc in
  if m.cpu.ca = cond then
    m.cpu.pc <- imm
  else
    incr_pc' m 2;
  12

let jp_z m cond =
  let imm = Mmu.get_u16 m m.cpu.pc in
  if m.cpu.z = cond then
    m.cpu.pc <- imm
  else
    incr_pc' m 2;
  12

let jp_nn m =
  m.cpu.pc <- Mmu.get_u16 m m.cpu.pc;
  12

let jp_hl m =
  m.cpu.pc <- get_register_pair m HL;
  8

let inc m reg =
  let r = get_register m reg in
  let res = Uint8.succ r in
  put_register m reg res;
  let hc = Uint8.((res land (Uint8.inj 0x0F)) < (r land (Uint8.inj 0x0F))) in
  put_flags m ~z:(res = Uint8.zero) ~n:false ~hc;
  4

let inc_p m p =
  get_register_pair m p |>
  Uint16.succ |>
  put_register_pair m p;
  4

let inc_sp m =
  m.cpu.sp <- (Uint16.add m.cpu.sp 1); 
  4

let dec_sp m =
  m.cpu.sp <- (Uint16.sub m.cpu.sp 1); 
  4

let dec m reg =
  let r = get_register m reg in
  let v = Uint8.pred r in
  put_register m reg v;
  let hc = Uint8.((v land (Uint8.inj 0x0F)) > (r land (Uint8.inj 0x0F))) in
  put_flags m ~z:(v = Uint8.zero) ~n:true ~hc;
  4

let dec_hl m =
  let a = get_register_pair m HL in
  let v = Mmu.get m a in
  let v' = Uint8.(pred v) in
  Mmu.put m a v';
  put_flags m ~z:(v' = Uint8.zero) ~n:true ~hc:Uint8.(v' land inj 0xf = inj 0xf);
  12

let inc_hl m =
  let a = get_register_pair m HL in
  let v = Mmu.get m a in 
  let v' = Uint8.(succ v) in
  Mmu.put m a v'; 
  put_flags m ~z:(v' = Uint8.zero) ~n:false ~hc:Uint8.(v' land inj 0xf = zero);
  12

let dec_p m p =
  let v = get_register_pair m p in
  let v' = Uint16.sub v 1 in
  put_register_pair m p v';
  8

let ld_p_nn m p reg =
  let hl = get_register_pair m p in
  get_register m reg |>
  Mmu.put m hl;
  8

let ld_r_hl m reg =
  let hl = get_register_pair m HL in
  get_register m reg |>
  Mmu.put m hl;
  8

let ldi_hl_r m reg =
  let hl = get_register_pair m HL in
  let v = Mmu.get m hl in
  put_register m reg v;
  put_register_pair m HL (Uint16.add hl 1);
  8

let ldi_r_hl m reg =
  let hl = get_register_pair m HL in
  get_register m reg |>
  Mmu.put m hl;
  put_register_pair m HL (Uint16.add hl 1);
  8

let ld_r_nn m reg =
  let nn = Mmu.get_u16 m m.cpu.pc in
  get_register m reg |>
  Mmu.put m nn;
  incr_pc' m 2;
  16

let ld_nn_r m reg =
  Mmu.get_u16 m m.cpu.pc |>
  Mmu.get m |>
  put_register m reg;
  incr_pc' m 2;
  16

let call_nn m =
  let nn = Mmu.get_u16 m m.cpu.pc in
  Mmu.put_u16 m (m.cpu.sp - 2) (m.cpu.pc + 2);
  m.cpu.pc <- nn;
  m.cpu.sp <- (m.cpu.sp - 2); 
  12

let call_z m cond =
  if m.cpu.z = cond then
    call_nn m
  else begin
    incr_pc' m 2;
    12
  end 

let call_c m cond =
  if m.cpu.ca = cond then
    call_nn m
  else begin
    incr_pc' m 2;
    12
  end 

let rst m n =
  Mmu.put_u16 m (m.cpu.sp - 2) m.cpu.pc;
  m.cpu.sp <- (Uint16.sub m.cpu.sp 2);
  m.cpu.pc <- n;
  12

let push_p m pair =
  get_register_pair m pair |>
  Mmu.put_u16 m (Uint16.sub m.cpu.sp 2);
  m.cpu.sp <- (Uint16.sub m.cpu.sp 2);
  16

let pop_p m pair =
  Mmu.get_u16 m m.cpu.sp |>
  put_register_pair m pair;
  m.cpu.sp <- (Uint16.add m.cpu.sp 2);
  12

let ret m =
  let pc = Mmu.get_u16 m m.cpu.sp in
  let sp = Uint16.add m.cpu.sp 2 in
  m.cpu.pc <- pc;
  m.cpu.sp <- sp;
  16

let reti m =
  m.cpu.pc <- Mmu.get_u16 m m.cpu.sp; 
  m.cpu.sp <- (Uint16.add m.cpu.sp 2);
  m.cpu.ime <- true;
  16

let ret_z m cond =
  if m.cpu.z = cond then
    ret m
  else
    12

let ret_c m cond =
  if m.cpu.ca = cond then
    ret m
  else
    12

let rlca m =
  let open Uint8 in
  let v = m.cpu.a in 
  let ca = is_bit_set v 7 in
  let v' = v lsl one in
  let v' = if ca then set_bit v' 0 else v' in
  put_flags m ~ca ~z:false ~n:false ~hc:false;
  put_register m A v';
  4

let rrca m =
  let open Uint8 in
  let v = m.cpu.a in 
  let c = v land one in
  let v' = (c lsl inj 7) lor (v lsr one) in
  let ca = if c = one then true else false in
  put_flags m ~ca ~z:false ~n:false ~hc:false;
  put_register m A v';
  4

let rlc_r m r =
  let open Uint8 in
  let v = get_register m r in 
  let ca = is_bit_set v 7 in
  let v' = v lsl one in
  let v' = if ca then set_bit v' 0 else v' in
  let z = v' = zero in
  put_flags m ~z ~ca ~n:false ~hc:false;
  put_register m r v';
  4

let rlc_hl m =
  let open Uint8 in
  let hl =  get_register_pair m HL in
  let v = Mmu.get m hl in 
  let ca = is_bit_set v 7 in
  let v' = v lsl one in
  let v' = if ca then set_bit v' 0 else v' in
  let z = v' = zero in
  put_flags m ~z ~ca ~n:false ~hc:false;
  Mmu.put m hl v';
  16

let rrc_r m r =
  let open Uint8 in
  let v = get_register m r in
  let ca = is_bit_set v 0 in
  let v' = v lsr one in
  let v' = if ca then set_bit v' 7 else v' in
  let z = v' = zero in
  put_flags m ~z ~ca ~n:false ~hc:false;
  put_register m r v';
  4

let rrc_hl m =
  let open Uint8 in
  let hl = get_register_pair m HL in 
  let v = Mmu.get m hl in
  let ca = is_bit_set v 0 in
  let v' = v lsr one in
  let v' = if ca then set_bit v' 7 else v' in
  let z = v' = zero in
  put_flags m ~z ~ca ~n:false ~hc:false;
  Mmu.put m hl v';
  16

let rl_hl m =
  let open Uint8 in
  let hl = get_register_pair m HL in 
  let r = Mmu.get m hl in
  let old_carry = m.cpu.ca in
  let r' = if old_carry then set_bit (r lsl one) 0 else r lsl one in
  Mmu.put m hl r';
  put_flags m ~z:(r' = zero) ~n:false ~hc:false ~ca:(is_bit_set r 7);
  16

let rl_r m reg =
  let open Uint8 in
  let r = get_register m reg in
  let old_carry = m.cpu.ca in
  let r' = if old_carry then set_bit (r lsl one) 0 else r lsl one in
  put_register m reg r';
  put_flags m ~z:(r' = zero) ~n:false ~hc:false ~ca:(is_bit_set r 7);
  8

let sra_hl m =
  let hl = get_register_pair m HL in
  let r = Mmu.get m hl in
  let ca = if Uint8.(r land one = one) then true else false in
  let v = Uint8.to_signed r lsr 1 in
  let v' = (v land 0xFF) |> Uint8.inj in
  let z = v' = Uint8.zero in
  Mmu.put m hl v';
  put_flags m ~z ~n:false ~hc:false ~ca;
  8

let sra_r m reg =
  let r = get_register m reg in
  let ca = if Uint8.(r land one = one) then true else false in
  let v = Uint8.to_signed r lsr 1 in
  let v' = (v land 0xFF) |> Uint8.inj in
  let z = v' = Uint8.zero in
  put_register m reg v';
  put_flags m ~z ~n:false ~hc:false ~ca;
  8

let rla m =
  let open Uint8 in
  let r = m.cpu.a in
  let ca = m.cpu.ca in
  let old_carry = if ca then one else zero in
  let ca = (r lsr inj 7) land one in
  let v' = (r lsl one) lor old_carry in
  let ca = if ca = one then true else false in
  m.cpu.a <- v';
  put_flags m ~z:false ~n:false ~hc:false ~ca;
  8

let cp_n m =
  let open Uint8 in
  let a = m.cpu.a in
  let n = get_pc m in
  let hc = (a land (Uint8.inj 0x0F)) < (n land (Uint8.inj 0x0F)) in
  put_flags m ~z:(n = a) ~n:true ~hc ~ca:(a < n);
  incr_pc m;
  8

let cp m r =
  let open Uint8 in
  let a = m.cpu.a in
  let n = get_register m r in
  let hc = (a land (Uint8.inj 0x0F)) < (n land (Uint8.inj 0x0F)) in
  put_flags m ~z:(n = a) ~n:true ~hc ~ca:(a < n);
  4

let cp_hl m =
  let open Uint8 in
  let a = m.cpu.a in
  let hl = get_register_pair m HL in
  let n = Mmu.get m hl in 
  let hc = (a land (Uint8.inj 0x0F)) < (n land (Uint8.inj 0x0F)) in
  put_flags m ~z:(n = a) ~n:true ~hc ~ca:(a < n);
  8

let sub_r m reg =
  let open Uint8 in
  let a = m.cpu.a in
  let r = get_register m reg in
  let res = sub a r in
  put_register m A res;
  let hc = (res land (inj 0x0F)) > (a land (inj 0x0F)) in
  put_flags m ~z:(a = r) ~hc ~n:true ~ca:(res > a);
  8

let sub_hl m =
  let open Uint8 in
  let a = m.cpu.a in
  let r =
    get_register_pair m HL |>
    Mmu.get m
  in 
  let res = sub a r in
  put_register m A res;
  let hc = (res land (inj 0x0F)) > (a land (inj 0x0F)) in
  put_flags m ~z:(a = r) ~hc ~n:true ~ca:(res > a);
  8

let add_hl m =
  let open Uint8 in
  let a = m.cpu.a in 
  let v =
    get_register_pair m HL |>
    Mmu.get m
  in 
  let res = add a v in
  put_register m A res;
  let hc = (res land (inj 0x0F)) < (a land (inj 0x0F)) in
  let ca = res < a in
  put_flags m ~n:false ~z:(res = zero) ~hc ~ca;
  8

let add m reg1 reg2 =
  let open Uint8 in
  let r1 = get_register m reg1 in 
  let r2 = get_register m reg2 in
  let res = add r1 r2 in
  put_register m reg2 res;
  let hc = (res land (inj 0x0F)) < (r2 land (inj 0x0F)) in
  let ca = res < r2 in
  put_flags m ~n:false ~z:(res = zero) ~hc ~ca;
  4

let add_n m =
  let open Uint8 in
  let r1 = m.cpu.a in
  let r2 = get_pc m in 
  let res = add r1 r2 in
  put_register m A res;
  let hc = (res land (inj 0x0F)) < (r2 land (inj 0x0F)) in
  let ca = res < r2 in
  put_flags m ~n:false ~z:(res = zero) ~hc ~ca;
  incr_pc m;
  4

let is_ca r1 r2 =
  let r1 = r1 land 0xFF in
  let r2 = r2 land 0xFF in
  r1 + r2 > 0xff

let is_hc r1 r2 =
  let r1 = r1 land 0xFF in
  let r2 = r2 land 0xFF in
  (r1 land 0xf) + (r2 land 0xf) > 0xf

let add_n_sp m =
  let r1 = m.cpu.sp in
  let r2 = Mmu.get m m.cpu.pc in 
  let r2 =
    if Uint8.proj r2 > 0x80 then
      -(0x80 - (Uint8.proj r2 - 0x80))
    else
      Uint8.proj r2
  in
  let sp = Uint16.add r1 r2 in
  let hc = is_hc r1 r2 in
  let ca = is_ca r1 r2 in
  put_flags m ~hc ~ca ~n:false ~z:false;
  m.cpu.sp <- sp;
  incr_pc m;
  4

let sub_n m =
  let open Uint8 in
  let a = m.cpu.a in
  let r = Mmu.get m m.cpu.pc in 
  let res = sub a r in
  m.cpu.a <- res;
  let hc = (res land (inj 0x0F)) > (a land (inj 0x0F)) in
  put_flags m ~z:(a = r) ~hc ~n:true ~ca:(res > a);
  incr_pc m;
  8

let res m bit reg =
  get_register m reg |>
  Uint8.unset_bit' bit |>
  put_register m reg;
  8 

let res_hl m bit =
  let hl = get_register_pair m HL in
  let v = Mmu.get m hl in 
  Mmu.put m hl (Uint8.unset_bit' bit v);
  8

let noop _ = 4

let di m =
  m.cpu.ime <- false;
  4

let ei m =
  m.cpu.ime <- true;
  4

let or' m reg1 reg2 =
  let open Uint8 in
  let r1 = get_register m reg1 in
  let r2 = get_register m reg2 in
  let v = r1 lor r2 in
  put_register m reg2 v;
  put_flags m ~z:(v = zero) ~n:false ~hc:false ~ca:false;
  4

let or_n m r =
  let open Uint8 in
  let r1 = get_register m r in
  let n = Mmu.get m m.cpu.pc in
  let v = n lor r1 in
  put_register m r v;
  put_flags m ~z:(v = zero) ~n:false ~hc:false ~ca:false;
  incr_pc m;
  4

let or_hl m =
  let open Uint8 in
  let hl = get_register_pair m HL in
  let r1 = m.cpu.a in
  let n = Mmu.get m hl in
  let v = n lor r1 in
  m.cpu.a <- v;
  put_flags m ~z:(v = zero) ~n:false ~hc:false ~ca:false;
  8

let and' m reg1 reg2 =
  let open Uint8 in
  let r1 = get_register m reg1 in
  let r2 = get_register m reg2 in
  let v = r1 land r2 in
  put_register m reg2 v;
  put_flags m ~z:(v = zero) ~n:false ~hc:true ~ca:false;
  4

let and_n m =
  let open Uint8 in
  let a = m.cpu.a in
  let n = Mmu.get m m.cpu.pc in 
  let v = n land a in
  put_register m A v;
  put_flags m ~z:(v = zero) ~n:false ~hc:true ~ca:false;
  incr_pc m;
  4

let and_hl m =
  let open Uint8 in
  let a = m.cpu.a in
  let n =
    get_register_pair m HL |>
    Mmu.get m
  in 
  let v = n land a in
  m.cpu.a <- v;
  put_flags m ~z:(v = zero) ~n:false ~hc:true ~ca:false;
  4

let cpl m =
  let open Uint8 in
  let v = m.cpu.a in
  let v' = lnot v in
  m.cpu.a <- v';
  put_flags m ~hc:true ~n:true;
  4

let scf m = put_flags m ~n:false ~hc:false ~ca:true; 4

let swap_hl m =
  let hl = get_register_pair m HL in
  let v = Mmu.get m hl in
  let v' = Uint8.swap_nibbles v in
  Mmu.put m hl v';
  put_flags m ~z:(v' = Uint8.zero) ~n:false ~hc:false ~ca:false;
  4

let swap m r =
  let v = get_register m r in
  let v' = Uint8.swap_nibbles v in
  put_register m r v';
  put_flags m ~z:(v' = Uint8.zero) ~n:false ~hc:false ~ca:false;
  4

(* let is_hc a b = Uint8.Infix.(((a land inj 0b1111) + (b land inj 0b1111)) land inj 0b10000) = inj 0b10000;; *)

let add_n_hl m p =
  let hl = get_register_pair m HL in 
  let p = get_register_pair m p in
  let res = hl + p in
  let hc = (hl land 0xfff) + (p land 0xfff) > 0xfff in
  put_register_pair m HL (res land Uint16.max_int);
  put_flags m ~n:false ~ca:(p + hl > 0xffff) ~hc;
  8

let add_sp_hl m =
  let hl = get_register_pair m HL in 
  let p = m.cpu.sp in
  let res = Uint16.add hl p in
  let hc = (hl land 0xfff) + (p land 0xfff) > 0xfff in
  put_flags m ~n:false ~ca:(p + hl > 0xffff) ~hc;
  put_register_pair m HL res;
  8

let ld_hl_r m r =
  let v = get_register_pair m HL |> Mmu.get m in
  put_register m r v;
  4

let sla_hl m =
  let open Uint8 in
  let hl = get_register_pair m HL in
  let v = Mmu.get m hl in
  let v' = v lsl one in
  put_flags m ~z:(v' = zero) ~hc:false ~n:false ~ca:(is_bit_set v 7);
  Mmu.put m hl v'; 
  8

let sla m r =
  let open Uint8 in
  let v = get_register m r in 
  let v' = v lsl one in
  put_flags m ~z:(v' = zero) ~hc:false ~n:false ~ca:(is_bit_set v 7);
  put_register m r v';
  8

(* FIXME: omg don't *)
let flags_sbc m reg carry n =
  let z = if Uint8.Infix.(reg - n - carry = Uint8.zero) then true else false in
  let hc =
    let open Uint8 in
    proj (reg land inj 0xf) - proj (n land inj 0xf) - proj carry < 0
  in
  let ca = if Uint8.proj reg - Uint8.proj n - Uint8.proj carry < 0 then true else false in
  put_flags m ~hc ~z ~n:true ~ca [@@inline]

let sbc m r1 r2 =
  let open Uint8 in
  let carry = if m.cpu.ca then one else zero in
  let v = get_register m r1 in  
  let n = get_register m r2 in 
  let v' = Infix.(v - n - carry) in
  put_register m r1 v';
  flags_sbc m v carry n;
  4

let sbc_hl m r1 =
  let open Uint8 in
  let carry = if m.cpu.ca then one else zero in
  let v = get_register m r1 in 
  let n = get_register_pair m HL |> Mmu.get m in 
  let v' = Infix.(v - n - carry) in
  put_register m r1 v';
  flags_sbc m v carry n;
  8

let sbc_n m r1 =
  let open Uint8 in
    let carry = if m.cpu.ca then one else zero in
  let v = get_register m r1 in 
  let n = Mmu.get m m.cpu.pc in
  let v' = Infix.(v - n - carry) in
  put_register m r1 v';
  flags_sbc m v carry n;
  incr_pc m;
  8

let ccf m =
  put_flags m ~hc:false ~n:false ~ca:(not m.cpu.ca);
  4

let daa m =
  let a = m.cpu.a in
  let adjust = ref 0 in

  if m.cpu.hc then
    adjust := !adjust lor 0x06;
  if m.cpu.ca then
    adjust := !adjust lor 0x60;

  let res =
    if m.cpu.n then
      Uint8.sub a (Uint8.inj !adjust)

     else begin
       if Uint8.proj a land 0x0F > 0x09 then
         adjust := !adjust lor 0x06;

       if Uint8.proj a > 0x99 then
         adjust := !adjust lor 0x60;

      Uint8.add a (Uint8.inj !adjust)
    end
   in
   let z = res = Uint8.zero in
   let ca = !adjust land 0x60 != 0 in
   let hc = false in
   put_flags m ~z ~hc ~ca;
   put_register m A res;
   4

let set m r i =
  let open Uint8 in
  get_register m r |> set_bit' i
  |> put_register m r;
  8

let set_hl m i =
  let open Uint8 in
  let hl = get_register_pair m HL in
  let v = Mmu.get m hl in
  Mmu.put m hl (set_bit v i);
  12

let bit_hl m i =
  let hl = get_register_pair m HL in 
  let v = Mmu.get m hl in 
  let z = not (Uint8.is_bit_set v i) in
  put_flags m ~z ~n:false ~hc:true;
  8

let srl m r =
  let open Uint8 in
  let v = get_register m r in 
  let v' = v lsr one in
  put_register m r v';
  put_flags m ~z:(v' = zero) ~n:false ~hc:false ~ca:(is_bit_set v 0);
  4

let srl_hl m =
  let open Uint8 in
  let hl = get_register_pair m HL in 
  let v = Mmu.get m hl in
  let v' = v lsr one in
  Mmu.put m hl v';
  put_flags m ~z:(v' = zero) ~n:false ~hc:false ~ca:(is_bit_set v 0); 
  8

let rr m re =
  let open Uint8 in
  let r = get_register m re in
  let old_carry = if m.cpu.ca then one else zero in
  let ca = if (r land one) = one then true else false in
  let v' = (old_carry lsl inj 7) lor (r lsr one) in
  put_register m re v';
  put_flags m ~n:false ~z:(v' = zero) ~hc:false ~ca;
  8

let rra m =
  let open Uint8 in
  let r = m.cpu.a in
  let old_carry = if m.cpu.ca then one else zero in
  let ca = if (r land one) = one then true else false in
  let v' = (old_carry lsl inj 7) lor (r lsr one) in
  m.cpu.a <- v';
  put_flags m ~n:false ~z:false ~hc:false ~ca;
  8

let rr_hl m =
  let open Uint8 in
  let hl = get_register_pair m HL in 
  let v = Mmu.get m hl in 
  let old_carry = if m.cpu.ca then one else zero in
  let ca = if (v land one) = one then true else false in
  let v' = (old_carry lsl inj 7) lor (v lsr one) in
  Mmu.put m hl v';
  put_flags m ~z:(v' = zero) ~n:false ~hc:false ~ca;
  8

let flags_adc m reg carry n =
  let z = if Uint8.Infix.(carry + reg + n = Uint8.zero) then true else false in
  let hc =
    let open Uint8 in
    let open Infix in
    (n land inj 0xf) + (reg land inj 0xf) + carry > inj 0xf
  in
  let ca = if Uint8.proj carry + Uint8.proj reg + Uint8.proj n > 0xff then true else false in
  put_flags m ~hc ~z ~n:false ~ca

let adc m r1 r2 =
  let open Uint8 in
  let v = get_register m r1 in
  let n = get_register m r2 in
  let carry = if m.cpu.ca then one else zero in
  let v' = Infix.(v + n + carry) in
  put_register m r1 v';
  flags_adc m v carry n;
  4

let adc_hl m r =
  let open Uint8 in
  let v = get_register m r in
  let n = get_register_pair m HL |> Mmu.get m in
  let carry = if m.cpu.ca then one else zero in
  let v' = Infix.(v + n + carry) in
  put_register m r v';
  flags_adc m v carry n;
  8

let adc_n m r =
  let open Uint8 in
  let v = get_register m r in
  let n = Mmu.get m m.cpu.pc in
  let carry = if m.cpu.ca then one else zero in
  let v' = Infix.(v + n + carry) in
  put_register m r v';
  flags_adc m v carry n;
  incr_pc m;
  8

let halt m =
  m.cpu.halted <- true;
  4

let dispatch (s : Machine.t) instr =
  match instr with
  | Instrs.Ld_n reg               -> ld_n s reg
  | Ldi_r_hl reg                  -> ldi_r_hl s reg
  | Ld_rr (r1, r2)                -> ld_rr s r1 r2
  | Ld_sp                         -> ld_sp s
  | Ld_p (pair, reg)              -> ld_p s pair reg
  | Ld_0xFF00_r_ra (reg1, reg2)   -> ld_0xFF00_r_ra s reg1 reg2
  | Ld_0xFF00_ra_r (reg1, reg2)   -> ld_0xFF00_ra_r s reg1 reg2
  | Ld_0xFF00_r_n reg             -> ld_0xFF00_r_n s reg
  | Ld_0xFF00_n_r reg             -> ld_0xFF00_n_r s reg
  | Ldi_hl_r reg                  -> ldi_hl_r s reg
  | Xor (reg1, reg2)              -> xor s reg1 reg2
  | Xor_n r                       -> xor_n s r
  | Xor_hl r                      -> xor_hl s r
  | Srl r                         -> srl s r
  | Srl_hl                        -> srl_hl s
  | Rra                           -> rra s
  | Rr r                          -> rr s r
  | Rr_hl                         -> rr_hl s
  | Ld_nn pair                    -> ld_nn s pair
  | Ldd (reg, pair)               -> ldd s reg pair
  | Ldd_hl (pair, reg)            -> ldd_hl s pair reg
  | Jr (flag, n)                  -> jr s flag n
  | Jrz (flag   )                 -> jrz s flag
  | Jr_n                          -> jr_n s
  | Jp_nn                         -> jp_nn s
  | Jp_z flag                     -> jp_z s flag
  | Jp_c flag                     -> jp_c s flag
  | Ld_hl_sp_n                    -> ld_hl_sp_n s
  | Ld_hl_sp                      -> ld_hl_sp s
  | Jp_hl                         -> jp_hl s
  | Inc reg                       -> inc s reg
  | Inc_sp                        -> inc_sp s
  | Dec_sp                        -> dec_sp s
  | Inc_p p                       -> inc_p s p
  | Dec reg                       -> dec s reg
  | Dec_p p                       -> dec_p s p
  | Or (r1, r2)                   -> or' s r1 r2
  | Or_n r1                       -> or_n s r1
  | Or_hl                         -> or_hl s
  | Ld_p_nn (p, reg)              -> ld_p_nn s p reg
  | Ld_nn_r reg                   -> ld_nn_r s reg
  | Ld_r_nn reg                   -> ld_r_nn s reg
  | Call_nn                       -> call_nn s
  | Call_z cond                   -> call_z s cond
  | Call_c cond                   -> call_c s cond
  | Push_p p                      -> push_p s p
  | Pop_p p                       -> pop_p s p
  | Ret                           -> ret s
  | Reti                          -> reti s
  | Ret_c cond                    -> ret_c s cond
  | Ret_z cond                    -> ret_z s cond
  | Rl_r reg                      -> rl_r s reg
  | Rl_hl                         -> rl_hl s
  | Rla                           -> rla s
  | Cp_n                          -> cp_n s
  | Cp_hl                         -> cp_hl s
  | Cp r                          -> cp s r
  | Sub_r reg                     -> sub_r s reg
  | Add_hl                        -> add_hl s
  | Res (bit, reg)                -> res s bit reg
  | Noop                          -> noop s
  | Ld_n_hl                       -> ld_n_hl s
  | Add (reg1, reg2)              -> add s reg1 reg2
  | Add_n                         -> add_n s
  | Sub_n                         -> sub_n s
  | Sub_hl                        -> sub_hl s
  | Di                            -> di s
  | Ei                            -> ei s
  | Adc (reg1, reg2)              -> adc s reg1 reg2
  | Set (r, i)                    -> set s r i
  | Set_hl i                      -> set_hl s i
  | Adc_n r                       -> adc_n s r
  | Adc_hl r                      -> adc_hl s r
  | Sbc_hl reg1                   -> sbc_hl s reg1
  | Sbc (reg1, reg2)              -> sbc s reg1 reg2
  | Sbc_n reg1                    -> sbc_n s reg1
  | Cpl                           -> cpl s
  | And (reg1, reg2)              -> and' s reg1 reg2
  | And_n                         -> and_n s
  | And_hl                        -> and_hl s
  | Add_n_hl p                    -> add_n_hl s p
  | Add_sp_hl                     -> add_sp_hl s
  | Scf                           -> scf s
  | Swap r                        -> swap s r
  | Rst n                         -> rst s n
  | Ld_hl_r r                     -> ld_hl_r s r
  | Dec_hl                        -> dec_hl s
  | Inc_hl                        -> inc_hl s
  | Ld_nn_sp                      -> ld_nn_sp s
  | Sla r                         -> sla s r
  | Bit (i, r)                    -> bit s r i
  | Bit_hl i                      -> bit_hl s i
  | Ccf                           -> ccf s
  | Daa                           -> daa s
  | Rlc                           -> rlca s
  | Rlc_hl                        -> rlc_hl s
  | Rrca                          -> rrca s
  | Rrc_r r                       -> rrc_r s r
  | Rrc_hl                        -> rrc_hl s
  | Rlc_n r                       -> rlc_r s r
  | Ld_r_hl r                     -> ld_r_hl s r
  | Add_n_sp                      -> add_n_sp s
  | Sra_r r                       -> sra_r s r
  | Sra_hl                        -> sra_hl s
  | Sla_hl                        -> sla_hl s
  | Swap_hl                       -> swap_hl s
  | Res_hl i                      -> res_hl s i
  | Halt                          -> halt s
  | Illegal                       -> 4
  | Unk s -> invalid_arg s  
  | _ -> assert false

