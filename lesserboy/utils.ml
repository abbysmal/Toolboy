let show_hex_i16 d = Printf.sprintf "%04x" d

let log (m : Machine.t) =
  let open Cpu in
  let pc = m.cpu.pc |> show_hex_i16 in
  let sp = m.cpu.sp |> show_hex_i16 in
  let a = Printf.sprintf "%02X" (Uint8.proj m.cpu.a) in
  let bc = get_register_pair m Cpu.BC |> show_hex_i16 in
  let de = get_register_pair m DE |> show_hex_i16 in
  let hl = get_register_pair m HL |> show_hex_i16 in
  let z = if m.cpu.z then 'Z' else '-' in
  let c = if m.cpu.ca then 'C' else '-' in
  let n = if m.cpu.n then 'N' else '-' in
  let h = if m.cpu.hc then 'H' else '-' in
  let i = Mmu.get_u16 m m.cpu.sp |> show_hex_i16 in
  let pccc = Printf.sprintf "%02X" (Mmu.get m (m.cpu.pc + 1) |> Uint8.proj) in 
  (* let ly = Uint8.proj m.gpu.ly in 
   * let lyc = Uint8.proj m.gpu.lyc in  *)
  Printf.printf "A:%s F:%c%c%c%c BC:%s DE:%s HL:%s SP:%s PC:%s I:%s %s\n" a z n h c bc de hl sp pc i pccc
