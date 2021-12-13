
let handle_interrupts (m : Machine.t) =
  let open Uint8 in
  let dispatch isf int handler =
    let sp = m.cpu.sp - 2 in
    Mmu.put_u16 m sp m.cpu.pc;
    let pc = handler in
    let halted = false in
    Mmu.put m 0xFF0F (unset_bit isf int);
    m.cpu.sp <- sp;
    m.cpu.pc <- pc;
    m.cpu.halted <- halted;
    m.cpu.ime <- false
  in
  match m.cpu.ime with
  | false -> ()
  | true ->
    let ise = Mmu.get m 0xFFFF in
    let isf = Mmu.get m 0xFF0F in
    let interrupt_on int ise isf =
      is_bit_set ise int
      &&
      is_bit_set isf int
    in
    if isf = zero then
      ()
    else
      if interrupt_on Interrupts.vblank ise isf then
        dispatch isf Interrupts.vblank Interrupts.vblank_handler
      else
      if interrupt_on Interrupts.lcd_stats ise isf then
        dispatch isf Interrupts.lcd_stats Interrupts.lcd_stats_handler
      else
      if interrupt_on Interrupts.timer ise isf then
        dispatch isf Interrupts.timer Interrupts.timer_handler
      else
      if interrupt_on Interrupts.joypad ise isf then
        dispatch isf Interrupts.joypad Interrupts.joypad_handler
      else
        ()

let step (m : Machine.t) =
  try
  let cur = Mmu.get m m.cpu.pc in
  if m.cpu.in_bios && m.cpu.pc = 0x100 then
    m.cpu.in_bios <- false;
  let cycles =
    match m.cpu.halted with
    | true -> 4
    | false ->
      try
        match Uint8.proj cur with
        | 0xCB ->
          let cur = Mmu.get m (m.cpu.pc + 1) in
          let op = Instrs.compile_extended cur in
          m.cpu.pc <- m.cpu.pc + 2;
          Alu.dispatch m op
        | code ->
          let op = Instrs.compile code in
          m.cpu.pc <- succ m.cpu.pc;
          Alu.dispatch m op
      with
      | exn ->
        failwith (Printf.sprintf "exception at %04X (%02X): %s" m.cpu.pc (Uint8.proj cur) (Printexc.to_string exn))
  in
  Cpu.tick m cycles;
  let tick = Timers.tick m (cycles / 4) in
  if tick then begin
    let isf = Mmu.get m 0xFF0F in
    Mmu.put m 0xFF0F (Uint8.set_bit isf Interrupts.timer)
  end;
  (try
    Gpu_exec.step m cycles
  with e -> print_endline "g"; raise e);
  if m.gpu.redraw then
    Timers.rtc_tick m;
  (try
     Spu.step m cycles;
  with e -> print_endline "s"; raise e);
  handle_interrupts m
  with
  | exn ->
    let cur = Mmu.get m m.cpu.pc in
    failwith (Printf.sprintf "exception at %04X (%02X): %s" m.cpu.pc (Uint8.proj cur) (Printexc.to_string exn))
