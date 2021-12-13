open Machine
    
let get_mode m =
  if Uint8.proj m.timers.tac land 4 != 0 then
    match Uint8.proj m.timers.tac land 3 with
    | 0 -> Some 64
    | 1 -> Some 1
    | 2 -> Some 4
    | 3 -> Some 16
    | _ -> None
  else
    None

let tick m m_cycles =
  m.timers.sub <- m.timers.sub + m_cycles;
  if m.timers.sub >= 4 then begin
    m.timers.main <- succ m.timers.main;
    m.timers.sub <- m.timers.sub - 4;
    m.timers.div_c <- succ m.timers.div_c;
    if m.timers.div_c == 16 then begin

      m.timers.div <- Uint8.succ m.timers.div;
      m.timers.div_c <- 0
    end
  end;
  match get_mode m with
  | None -> false
  | Some threshold when m.timers.main > threshold -> begin
      m.timers.main <- 0;
      m.timers.tima <- Uint8.succ m.timers.tima;
      if m.timers.tima = Uint8.zero then begin
        m.timers.tima <- m.timers.tma;
        true
      end
      else
        false
    end
  | _ -> false

let rtc_tick m =
  match m.timers.rtc_real with
  | Some rtc_real -> begin
    let time = Sys.time () |> int_of_float in
    let aux time =
      let open Uint8 in
      if m.timers.last_rtc < time then (
        m.timers.last_rtc <- m.timers.last_rtc + 1;
        rtc_real.seconds <- add rtc_real.seconds one;
        if rtc_real.seconds == (inj 60) then (
          rtc_real.seconds <- zero;
          rtc_real.minutes <- add rtc_real.minutes one;
          if rtc_real.minutes == (inj 60) then (
            rtc_real.minutes <- zero;
            rtc_real.minutes <- add rtc_real.minutes one;
            if rtc_real.hours == (inj 24) then (
              rtc_real.hours <- zero;
              rtc_real.days <- add rtc_real.days one;
              if rtc_real.days == zero then (
                if (rtc_real.high land one) != zero then
                  rtc_real.high <- rtc_real.high lor inj 0x80;
                rtc_real.high <- rtc_real.high lxor one; 
              )
            )
          )
        )
      )
    in
    aux time
  end
  | None -> ()
